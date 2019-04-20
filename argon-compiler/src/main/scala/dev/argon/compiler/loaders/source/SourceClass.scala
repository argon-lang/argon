package dev.argon.compiler.loaders.source

import dev.argon.compiler._
import dev.argon.compiler.core.PayloadSpecifiers.DeclarationPayloadSpecifier
import dev.argon.compiler.core.{ArClass, _}
import dev.argon.compiler.loaders.source.ExpressionConverter.EnvCreator
import dev.argon.parser
import dev.argon.parser.ClassDeclarationStmt
import dev.argon.util.{SourceLocation, WithSource}
import scalaz._
import Scalaz._
import dev.argon.compiler.loaders.source.SourceSignatureCreator.ResultCreator

private[compiler] object SourceClass extends AccessModifierHelpers {

  final case class GroupedStaticStatements
  (
    staticMethods: Vector[WithSource[parser.MethodDeclarationStmt]],
    classCtors: Vector[WithSource[parser.ClassConstructorDeclarationStmt]],
  )

  final case class GroupedInstanceStatements
  (
    methods: Vector[WithSource[parser.MethodDeclarationStmt]],
    fields: Vector[WithSource[parser.FieldDeclarationStmt]],
  )

  def apply[TComp[+_] : Compilation]
  (context2: ContextComp[TComp])
  (env: EnvCreator[context2.type])
  (stmt: ClassDeclarationStmt)
  (desc: ClassDescriptor)
  : TComp[ArClass[context2.type, PayloadSpecifiers.DeclarationPayloadSpecifier] { val descriptor: desc.type }] = for {
    sigCache <- Compilation[TComp].createCache[context2.signatureContext.Signature[ArClass.ResultInfo]]

    groupedStaticCache <- Compilation[TComp].createCache[GroupedStaticStatements]
    groupedInstCache <- Compilation[TComp].createCache[GroupedInstanceStatements]

    fieldCache <- Compilation[TComp].createCache[Vector[context2.typeSystem.FieldVariable]]
    methodCache <- Compilation[TComp].createCache[Vector[MethodBinding[context2.type, DeclarationPayloadSpecifier]]]
    staticMethodCache <- Compilation[TComp].createCache[Vector[MethodBinding[context2.type, DeclarationPayloadSpecifier]]]
    classCtorCache <- Compilation[TComp].createCache[Vector[ClassConstructorBinding[context2.type, DeclarationPayloadSpecifier]]]

  } yield new ArClass[context2.type, PayloadSpecifiers.DeclarationPayloadSpecifier] {
    override val context: context2.type = context2
    import context.signatureContext.Signature

    override val contextProof: Leibniz[context.type, context2.type, context.type, context2.type] = Leibniz.refl

    override val descriptor: desc.type = desc

    override val isSealed: Boolean = stmt.modifiers.exists {
      case WithSource(parser.SealedModifier, _) => true
      case _ => false
    }
    override val isOpen: Boolean = stmt.modifiers.exists {
      case WithSource(parser.OpenModifier, _) => true
      case _ => false
    }
    override val isAbstract: Boolean = stmt.modifiers.exists {
      case WithSource(parser.AbstractModifier, _) => true
      case _ => false
    }

    private val groupedStatic =
      groupedStaticCache(
        stmt.body.foldLeftM(GroupedStaticStatements(Vector.empty, Vector.empty)) {
          case (group, WithSource(stmt: parser.MethodDeclarationStmt, location)) =>
            group.copy(staticMethods = group.staticMethods :+ WithSource(stmt, location)).point[TComp]

          case (group, WithSource(stmt: parser.ClassConstructorDeclarationStmt, location)) =>
            group.copy(classCtors = group.classCtors :+ WithSource(stmt, location)).point[TComp]

          case (_, WithSource(_, location)) =>
            Compilation[TComp].forErrors(CompilationError.UnexpectedStatement(CompilationMessageSource.SourceFile(env.fileSpec, location)))
        }
      )

    private val groupedInst =
      groupedInstCache(
        stmt.instanceBody.foldLeftM(GroupedInstanceStatements(Vector.empty, Vector.empty)) {
          case (group, WithSource(stmt: parser.MethodDeclarationStmt, location)) =>
            group.copy(methods = group.methods :+ WithSource(stmt, location)).point[TComp]

          case (group, WithSource(stmt: parser.FieldDeclarationStmt, location)) =>
            group.copy(fields = group.fields :+ WithSource(stmt, location)).point[TComp]

          case (_, WithSource(_, location)) =>
            Compilation[TComp].forErrors(CompilationError.UnexpectedStatement(CompilationMessageSource.SourceFile(env.fileSpec, location)))
        }
      )

    override val signature: TComp[Signature[ArClass.ResultInfo]] =
      sigCache(
        SourceSignatureCreator.fromParameters[TComp, ArClass.ResultInfo](context2)(
          env(context)(EffectInfo.pure, descriptor)
        )(descriptor)(stmt.parameters)(resultCreator(stmt.baseType))
      )

    override val fields: TComp[Vector[context.typeSystem.FieldVariable]] =
      fieldCache(groupedInst.flatMap { inst =>
        inst.fields.traverse { field =>
          field.value.name match {
            case Some(fieldName) =>
              ExpressionConverter.convertTypeExpression(context)(env(context)(EffectInfo.pure, descriptor))(field.value.fieldType).map { fieldType =>
                context.typeSystem.FieldVariable(
                  FieldDescriptor(descriptor, fieldName),
                  AbsRef(this),
                  VariableName.Normal(fieldName),
                  Mutability.fromIsMutable(field.value.isMutable),
                  fieldType
                )
              }

            case None =>
              Compilation[TComp].forErrors(CompilationError.FieldMustHaveName(CompilationMessageSource.SourceFile(env.fileSpec, field.location)))
          }
        }
      })

    override val methods: TComp[Vector[MethodBinding[context2.type, DeclarationPayloadSpecifier]]] =
      methodCache(groupedInst.flatMap { inst =>
        inst.methods.zipWithIndex.traverse { case (method, i) =>
          parseAccessModifier(env.fileSpec, method.location, getAccessModifiers(method.value.modifiers)).flatMap { modifiers =>
            val memberName = method.value.name match {
              case Some(name) => MemberName.Normal(name)
              case None => MemberName.Unnamed
            }

            fields.flatMap { fieldVars =>
              val env2 = env.addVariables(context)(fieldVars)
              val desc = MethodDescriptor(descriptor, i, memberName)
              SourceMethod(context)(env2)(method.value, method.location)(desc)(ArMethod.ClassOwner(this))
                .map(MethodBinding(memberName, i, modifiers, _))
            }
          }
        }
      })

    override val staticMethods: TComp[Vector[MethodBinding[context2.type, DeclarationPayloadSpecifier]]] =
      staticMethodCache(groupedStatic.flatMap { statics =>
        statics.staticMethods.zipWithIndex.traverse { case (method, i) =>
          parseAccessModifier(env.fileSpec, method.location, getAccessModifiers(method.value.modifiers)).flatMap { modifiers =>
            val memberName = method.value.name match {
              case Some(name) => MemberName.Normal(name)
              case None => MemberName.Unnamed
            }

            val desc = MethodDescriptor(ClassObjectDescriptor(descriptor), i, memberName)
            SourceMethod(context)(env)(method.value, method.location)(desc)(ArMethod.ClassObjectOwner(this))
              .map(MethodBinding(memberName, i, modifiers, _))
          }
        }
      })

    override val classConstructors: TComp[Vector[ClassConstructorBinding[context2.type, DeclarationPayloadSpecifier]]] =
      classCtorCache(groupedStatic.flatMap { statics =>
        statics.classCtors.zipWithIndex.traverse { case (classCtor, i) =>
          parseAccessModifier(env.fileSpec, classCtor.location, getAccessModifiers(classCtor.value.modifiers)).flatMap { modifiers =>
            val desc = ClassConstructorDescriptor(descriptor, i)
            SourceClassConstructor(context)(env)(this)(classCtor.value)(desc).map(ClassConstructorBinding(i, modifiers, _))
          }
        }
      })


    override val payload: Unit = ()
  }

  private def resultCreator(baseTypeExpr: Option[WithSource[parser.Expr]]): ResultCreator[ArClass.ResultInfo] = new ResultCreator[ArClass.ResultInfo] {
    override def createResult[TComp[+ _] : Compilation]
    (context: ContextComp[TComp])
    (env: ExpressionConverter.Env[context.type, context.scopeContext.Scope])
    : TComp[ArClass.ResultInfo[context.type, context.typeSystem.type]] =
      (baseTypeExpr match {
        case Some(baseTypeExpr) =>
          ExpressionConverter.convertTypeExpression(context)(env)(baseTypeExpr)
            .flatMap(typeToBaseTypes(context)(env)(_)(baseTypeExpr.location)(context.typeSystem.BaseTypeInfoClass(None, Vector())))
        case None =>
          context.typeSystem.BaseTypeInfoClass(None, Vector()).point[TComp]
      })
        .map { baseTypes => ArClass.ResultInfo(context.typeSystem)(baseTypes) }

    private def typeToBaseTypes[TComp[+ _] : Compilation]
    (context: ContextComp[TComp])
    (env: ExpressionConverter.Env[context.type, context.scopeContext.Scope])
    (t: context.typeSystem.TType)
    (location: SourceLocation)
    (acc: context.typeSystem.BaseTypeInfoClass)
    : TComp[context.typeSystem.BaseTypeInfoClass] =
      t match {
        case t: context.typeSystem.ClassType =>
          if(acc.baseClass.isDefined)
            Compilation[TComp].forErrors(CompilationError.MultipleBaseClasses(CompilationMessageSource.SourceFile(env.fileSpec, location)))
          else
            acc.copy(baseClass = Some(t)).point[TComp]

        case t: context.typeSystem.TraitType =>
          acc.copy(baseTraits = acc.baseTraits :+ t).point[TComp]

        case context.typeSystem.IntersectionType(first, second) =>
          typeToBaseTypes(context)(env)(first)(location)(acc).flatMap { acc2 =>
            typeToBaseTypes(context)(env)(second)(location)(acc2)
          }

        case _ =>
          Compilation[TComp].forErrors(CompilationError.InvalidBaseType(CompilationMessageSource.SourceFile(env.fileSpec, location)))
      }
  }

}
