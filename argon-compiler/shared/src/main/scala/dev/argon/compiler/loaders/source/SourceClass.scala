package dev.argon.compiler.loaders.source

import dev.argon.compiler._
import dev.argon.compiler.core.PayloadSpecifiers.DeclarationPayloadSpecifier
import dev.argon.compiler.core.{ArClass, _}
import dev.argon.compiler.loaders.source.ExpressionConverter.EnvCreator
import dev.argon.parser
import dev.argon.parser.ClassDeclarationStmt
import dev.argon.util.{FileID, SourceLocation, WithSource}
import cats._
import cats.implicits._
import cats.evidence.Is
import dev.argon.compiler.loaders.source.SourceSignatureCreator.ResultCreator
import shapeless.Nat

private[compiler] object SourceClass extends AccessModifierHelpers {

  final case class GroupedStaticStatements
  (
    staticMethods: Vector[WithSource[parser.MethodDeclarationStmt]],
  )

  final case class GroupedInstanceStatements
  (
    methods: Vector[WithSource[parser.MethodDeclarationStmt]],
    fields: Vector[WithSource[parser.FieldDeclarationStmt]],
    classCtors: Vector[WithSource[parser.ClassConstructorDeclarationStmt]],
  )

  def apply
  (context2: Context)
  (env: EnvCreator[context2.type])
  (stmt: ClassDeclarationStmt)
  (desc: ClassDescriptor)
  : context2.Comp[ArClass[context2.type, PayloadSpecifiers.DeclarationPayloadSpecifier] { val descriptor: desc.type }] = {
    import context2._

    for {
      sigCache <- Compilation[Comp].createCache[context2.signatureContext.Signature[ArClass.ResultInfo, _ <: Nat]]

      paramsEnvCache <- Compilation[Comp].createCache[EnvCreator[context2.type]]

      groupedStaticCache <- Compilation[Comp].createCache[GroupedStaticStatements]
      groupedInstCache <- Compilation[Comp].createCache[GroupedInstanceStatements]

      fieldCache <- Compilation[Comp].createCache[Vector[context2.typeSystem.FieldVariable]]
      methodCache <- Compilation[Comp].createCache[Vector[MethodBinding[context2.type, DeclarationPayloadSpecifier]]]
      staticMethodCache <- Compilation[Comp].createCache[Vector[MethodBinding[context2.type, DeclarationPayloadSpecifier]]]
      classCtorCache <- Compilation[Comp].createCache[Vector[ClassConstructorBinding[context2.type, DeclarationPayloadSpecifier]]]

    } yield new ArClass[context2.type, PayloadSpecifiers.DeclarationPayloadSpecifier] with OpenSealedCheck {
      override val context: context2.type = context2
      import context.signatureContext.Signature

      override val contextProof: context.type Is context2.type = Is.refl

      override val descriptor: desc.type = desc
      override val fileId: FileID = env.fileSpec.fileID
      override val classMessageSource: CompilationMessageSource = CompilationMessageSource.SourceFile(env.fileSpec, stmt.name.location)

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
          stmt.body.foldLeftM(GroupedStaticStatements(Vector.empty)) {
            case (group, WithSource(stmt: parser.MethodDeclarationStmt, location)) =>
              group.copy(staticMethods = group.staticMethods :+ WithSource(stmt, location)).pure[Comp]

            case (_, WithSource(_, location)) =>
              Compilation[Comp].forErrors(CompilationError.UnexpectedStatement(CompilationMessageSource.SourceFile(env.fileSpec, location)))
          }
        )

      private val groupedInst =
        groupedInstCache(
          stmt.instanceBody.foldLeftM(GroupedInstanceStatements(Vector.empty, Vector.empty, Vector.empty)) {
            case (group, WithSource(stmt: parser.MethodDeclarationStmt, location)) =>
              group.copy(methods = group.methods :+ WithSource(stmt, location)).pure[Comp]

            case (group, WithSource(stmt: parser.FieldDeclarationStmt, location)) =>
              group.copy(fields = group.fields :+ WithSource(stmt, location)).pure[Comp]

            case (group, WithSource(stmt: parser.ClassConstructorDeclarationStmt, location)) =>
              group.copy(classCtors = group.classCtors :+ WithSource(stmt, location)).pure[Comp]

            case (_, WithSource(_, location)) =>
              Compilation[Comp].forErrors(CompilationError.UnexpectedStatement(CompilationMessageSource.SourceFile(env.fileSpec, location)))
          }
        )

      override val signature: Comp[Signature[ArClass.ResultInfo, _ <: Nat]] =
        sigCache(
          SourceSignatureCreator.fromParameters[ArClass.ResultInfo](context2)(
            env(context)(EffectInfo.pure, descriptor)
          )(descriptor)(stmt.parameters)(resultCreator(stmt.baseType)(this))
        )

      private val paramsEnv: Comp[EnvCreator[context.type]] =
        paramsEnvCache(
          signature.map { sig =>
            env.addParameters(context)(sig.unsubstitutedParameters)
          }
        )

      override val fields: Comp[Vector[context.typeSystem.FieldVariable]] =
        fieldCache(groupedInst.flatMap { inst =>
          inst.fields.traverse { field =>
            field.value.name match {
              case Some(fieldName) =>
                for {
                  env2 <- paramsEnv
                  fieldType <- ExpressionConverter.convertTypeExpression(context)(env2(context)(EffectInfo.pure, descriptor))(field.value.fieldType)
                } yield context.typeSystem.FieldVariable(
                  FieldDescriptor(descriptor, fieldName),
                  AbsRef(this),
                  VariableName.Normal(fieldName),
                  Mutability.fromIsMutable(field.value.isMutable),
                  fieldType
                )

              case None =>
                Compilation[Comp].forErrors(CompilationError.FieldMustHaveName(CompilationMessageSource.SourceFile(env.fileSpec, field.location)))
            }
          }
        })

      override val methods: Comp[Vector[MethodBinding[context2.type, DeclarationPayloadSpecifier]]] =
        methodCache(groupedInst.flatMap { inst =>
          inst.methods.zipWithIndex.traverse { case (method, i) =>
            parseAccessModifier[Comp](env.fileSpec, method.location, getAccessModifiers(method.value.modifiers)).flatMap { modifiers =>
              val memberName = method.value.name match {
                case Some(name) => MemberName.Normal(name)
                case None => MemberName.Unnamed
              }

              paramsEnv.flatMap { env2 =>
                fields.flatMap { fieldVars =>
                  val env3 = env2.addVariables(context)(fieldVars)
                  val desc = MethodDescriptor(descriptor, i, memberName)
                  SourceMethod(context)(env3)(method.value, method.location)(desc)(ArMethod.ClassOwner(this))
                    .map(MethodBinding(memberName, i, modifiers, _))
                }
              }
            }
          }
        })

      override val staticMethods: Comp[Vector[MethodBinding[context2.type, DeclarationPayloadSpecifier]]] =
        staticMethodCache(groupedStatic.flatMap { statics =>
          statics.staticMethods.zipWithIndex.traverse { case (method, i) =>
            parseAccessModifier[Comp](env.fileSpec, method.location, getAccessModifiers(method.value.modifiers)).flatMap { modifiers =>
              val memberName = method.value.name match {
                case Some(name) => MemberName.Normal(name)
                case None => MemberName.Unnamed
              }
              val desc = MethodDescriptor(ClassObjectDescriptor(descriptor), i, memberName)

              paramsEnv.flatMap { env2 =>
                SourceMethod(context)(env2)(method.value, method.location)(desc)(ArMethod.ClassObjectOwner(this))
                  .map(MethodBinding(memberName, i, modifiers, _))

              }
            }
          }
        })

      override val classConstructors: Comp[Vector[ClassConstructorBinding[context2.type, DeclarationPayloadSpecifier]]] =
        classCtorCache(groupedInst.flatMap { inst =>
          inst.classCtors.zipWithIndex.traverse { case (classCtor, i) =>
            parseAccessModifier[Comp](env.fileSpec, classCtor.location, getAccessModifiers(classCtor.value.modifiers)).flatMap { modifiers =>
              val desc = ClassConstructorDescriptor(descriptor, i)

              paramsEnv.flatMap { env2 =>
                SourceClassConstructor(context)(env2)(this)(classCtor.value)(desc).map(ClassConstructorBinding(i, modifiers, _))
              }
            }
          }
        })


      override val payload: Unit = ()
    }
  }

  private def resultCreator(baseTypeExpr: Option[WithSource[parser.Expr]])(osCheck: OpenSealedCheck): ResultCreator[ArClass.ResultInfo] = new ResultCreator[ArClass.ResultInfo] {
    override def createResult
    (context: Context)
    (env: ExpressionConverter.Env[context.type, context.scopeContext.Scope])
    : context.Comp[ArClass.ResultInfo[context.type, context.typeSystem.type]] = {
      import context._

      (baseTypeExpr match {
        case Some(baseTypeExpr) =>
          ExpressionConverter.convertTypeExpression(context)(env)(baseTypeExpr)
            .flatMap(typeToBaseTypes(context)(env)(_)(baseTypeExpr.location)(context.typeSystem.BaseTypeInfoClass(None, Vector())))
            .flatMap { baseTypes =>
              val messageSource = CompilationMessageSource.SourceFile(env.fileSpec, baseTypeExpr.location)

              baseTypes.baseClass.traverse_ { baseClass =>
                osCheck.checkExtendClass[Comp, context.type, baseClass.arClass.PayloadSpec](baseClass.arClass.value)(messageSource)
              }
                .flatMap { _ =>
                  baseTypes.baseTraits.traverse_ { baseTrait =>
                    osCheck.checkExtendTrait[Comp, context.type, baseTrait.arTrait.PayloadSpec](baseTrait.arTrait.value)(messageSource)
                  }
                }
                .map { _ => baseTypes }
            }
        case None =>
          context.typeSystem.BaseTypeInfoClass(None, Vector()).pure[Comp]
      })
        .map { baseTypes => ArClass.ResultInfo(context.typeSystem)(baseTypes) }
    }

    private def typeToBaseTypes
    (context: Context)
    (env: ExpressionConverter.Env[context.type, context.scopeContext.Scope])
    (t: context.typeSystem.TType)
    (location: SourceLocation)
    (acc: context.typeSystem.BaseTypeInfoClass)
    : context.Comp[context.typeSystem.BaseTypeInfoClass] = {
      import context._
      t match {
        case t: context.typeSystem.ClassType =>
          if(acc.baseClass.isDefined)
            Compilation[Comp].forErrors(CompilationError.MultipleBaseClasses(CompilationMessageSource.SourceFile(env.fileSpec, location)))
          else
            acc.copy(baseClass = Some(t)).pure[Comp]

        case t: context.typeSystem.TraitType =>
          acc.copy(baseTraits = acc.baseTraits :+ t).pure[Comp]

        case context.typeSystem.IntersectionType(first, second) =>
          typeToBaseTypes(context)(env)(first)(location)(acc).flatMap { acc2 =>
            typeToBaseTypes(context)(env)(second)(location)(acc2)
          }

        case _ =>
          Compilation[Comp].forErrors(CompilationError.InvalidBaseType(CompilationMessageSource.SourceFile(env.fileSpec, location)))
      }
    }
  }

}
