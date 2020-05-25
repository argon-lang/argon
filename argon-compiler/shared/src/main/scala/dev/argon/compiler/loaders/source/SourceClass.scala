package dev.argon.compiler.loaders.source

import dev.argon.compiler._
import dev.argon.compiler.core.PayloadSpecifiers.DeclarationPayloadSpecifier
import dev.argon.compiler.core.{ArClass, _}
import dev.argon.compiler.loaders.source.ExpressionConverter.EnvCreator
import dev.argon.parser
import dev.argon.parser.ClassDeclarationStmt
import dev.argon.util.{FileID, SourceLocation, UniqueIdentifier, ValueCache, WithSource}
import cats.{Id => _, _}
import cats.implicits._
import cats.evidence.Is
import dev.argon.compiler.expr.ArExpr._
import dev.argon.compiler.expr._
import dev.argon.compiler.loaders.source.SourceSignatureCreator.ResultCreator
import shapeless.{Id, Nat}
import zio.{IO, ZIO}
import zio.interop.catz.core._

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
  (classOwner: ClassOwner)
  : Comp[ArClass[context2.type, PayloadSpecifiers.DeclarationPayloadSpecifier] { val owner: classOwner.type }] = {
    import context2._

    for {
      unqId <- UniqueIdentifier.make

      sigCache <- ValueCache.make[ErrorList, context2.signatureContext.Signature[ArClass.ResultInfo, _ <: Nat]]
      sigResultCache <- ValueCache.make[ErrorList, BaseTypeInfoClass[context2.type, Id]]

      paramsEnvCache <- ValueCache.make[ErrorList, EnvCreator[context2.type]]

      groupedStaticCache <- ValueCache.make[ErrorList, GroupedStaticStatements]
      groupedInstCache <- ValueCache.make[ErrorList, GroupedInstanceStatements]

      fieldCache <- ValueCache.make[ErrorList, Vector[FieldVariable[context2.type, Id]]]
      methodCache <- ValueCache.make[ErrorList, Vector[MethodBinding[context2.type, DeclarationPayloadSpecifier]]]
      staticMethodCache <- ValueCache.make[ErrorList, Vector[MethodBinding[context2.type, DeclarationPayloadSpecifier]]]
      classCtorCache <- ValueCache.make[ErrorList, Vector[ClassConstructorBinding[context2.type, DeclarationPayloadSpecifier]]]

    } yield new ArClass[context2.type, PayloadSpecifiers.DeclarationPayloadSpecifier] with OpenSealedCheck {
      override val context: context2.type = context2
      import context.signatureContext.Signature

      override val contextProof: context.type Is context2.type = Is.refl


      override val id: ClassId = ClassId(unqId)
      override val owner: classOwner.type = classOwner
      override def ownerModuleId: ModuleId = getClassModule(this)
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

      private def localVarOwner = LocalVariableOwner.ByClass(AbsRef(this))
      private def paramVarOwner = ParameterVariableOwner.ByClass(AbsRef(this))

      private val groupedStatic =
        groupedStaticCache.get(
          ZIO.foldLeft(stmt.body)(GroupedStaticStatements(Vector.empty)) {
            case (group, WithSource(stmt: parser.MethodDeclarationStmt, location)) =>
              IO.succeed(group.copy(staticMethods = group.staticMethods :+ WithSource(stmt, location)))

            case (_, WithSource(_, location)) =>
              Compilation.forErrors(CompilationError.UnexpectedStatement(CompilationMessageSource.SourceFile(env.fileSpec, location)))
          }
        )

      private val groupedInst =
        groupedInstCache.get(
          ZIO.foldLeft(stmt.instanceBody)(GroupedInstanceStatements(Vector.empty, Vector.empty, Vector.empty)) {
            case (group, WithSource(stmt: parser.MethodDeclarationStmt, location)) =>
              IO.succeed(group.copy(methods = group.methods :+ WithSource(stmt, location)))

            case (group, WithSource(stmt: parser.FieldDeclarationStmt, location)) =>
              IO.succeed(group.copy(fields = group.fields :+ WithSource(stmt, location)))

            case (group, WithSource(stmt: parser.ClassConstructorDeclarationStmt, location)) =>
              IO.succeed(group.copy(classCtors = group.classCtors :+ WithSource(stmt, location)))

            case (_, WithSource(_, location)) =>
              Compilation.forErrors(CompilationError.UnexpectedStatement(CompilationMessageSource.SourceFile(env.fileSpec, location)))
          }
        )

      override val signature: Comp[Signature[ArClass.ResultInfo, _ <: Nat]] =
        sigCache.get(
          SourceSignatureCreator.fromParameters[ArClass.ResultInfo](context2)(
            env(context)(EffectInfo.pure, id, localVarOwner)
          )(paramVarOwner)(stmt.parameters)(resultCreator(context)(stmt.baseType, sigResultCache)(this))
        )

      private val paramsEnv: Comp[EnvCreator[context.type]] =
        paramsEnvCache.get(
          signature.map { sig =>
            env.addParameters(context)(sig.unsubstitutedParameters)
          }
        )

      override val fields: Comp[Vector[FieldVariable[context.type, Id]]] =
        fieldCache.get(groupedInst.flatMap { inst =>
          inst.fields.traverse { field =>
            field.value.name match {
              case Some(fieldName) =>
                for {
                  env2 <- paramsEnv
                  fieldType <- ExpressionConverter.convertTypeExpression(context)(env2(context)(EffectInfo.pure, id, localVarOwner))(field.value.fieldType)
                } yield FieldVariable[context.type, Id](
                  FieldVariableOwner(AbsRef(this)),
                  VariableName.Normal(fieldName),
                  Mutability.fromIsMutable(field.value.isMutable),
                  fieldType
                )

              case None =>
                Compilation.forErrors(CompilationError.FieldMustHaveName(CompilationMessageSource.SourceFile(env.fileSpec, field.location)))
            }
          }
        })

      override val methods: Comp[Vector[MethodBinding[context2.type, DeclarationPayloadSpecifier]]] =
        methodCache.get(groupedInst.flatMap { inst =>
          inst.methods.zipWithIndex.traverse { case (method, i) =>
            parseAccessModifier(env.fileSpec, method.location, getAccessModifiers(method.value.modifiers)).flatMap { modifiers =>
              paramsEnv.flatMap { env2 =>
                fields.flatMap { fieldVars =>
                  val env3 = env2.addVariables(context)(fieldVars)
                  SourceMethod(context)(env3)(method.value, method.location)(MethodOwner.ByClass(this))
                    .map { method =>
                      MethodBinding(method.name, i, modifiers, method)
                    }
                }
              }
            }
          }
        })

      override val staticMethods: Comp[Vector[MethodBinding[context2.type, DeclarationPayloadSpecifier]]] =
        staticMethodCache.get(groupedStatic.flatMap { statics =>
          statics.staticMethods.zipWithIndex.traverse { case (method, i) =>
            parseAccessModifier(env.fileSpec, method.location, getAccessModifiers(method.value.modifiers)).flatMap { modifiers =>
              paramsEnv.flatMap { env2 =>
                SourceMethod(context)(env2)(method.value, method.location)(MethodOwner.ByClassObject(this))
                  .map { method =>
                    MethodBinding(method.name, i, modifiers, method)
                  }
              }
            }
          }
        })

      override val classConstructors: Comp[Vector[ClassConstructorBinding[context2.type, DeclarationPayloadSpecifier]]] =
        classCtorCache.get(groupedInst.flatMap { inst =>
          inst.classCtors.traverse { classCtor =>
            parseAccessModifier(env.fileSpec, classCtor.location, getAccessModifiers(classCtor.value.modifiers)).flatMap { modifiers =>
              paramsEnv.flatMap { env2 =>
                SourceClassConstructor(context)(env2)(this)(classCtor.value).map(ClassConstructorBinding(modifiers, _))
              }
            }
          }
        })


      override val payload: Unit = ()
    }
  }

  private def resultCreator(ctx: Context)(baseTypeExpr: Option[WithSource[parser.Expr]], cache: ValueCache[ErrorList, BaseTypeInfoClass[ctx.type, Id]])(osCheck: OpenSealedCheck): ResultCreator.Aux[ctx.type, ArClass.ResultInfo] = new ResultCreator[ArClass.ResultInfo] {

    override val context: ctx.type = ctx

    override def createResult
    (env: ExpressionConverter.Env[context.type, context.scopeContext.Scope])
    : Comp[ArClass.ResultInfo[context.type, context.typeSystem.TTypeWrapper]] = {

      ArClass.ResultInfo(
        cache.get(baseTypeExpr match {
          case Some(baseTypeExpr) =>
            ExpressionConverter.convertTypeExpression(context)(env)(baseTypeExpr)
              .flatMap(typeToBaseTypes(context)(env)(_)(baseTypeExpr.location)(BaseTypeInfoClass(None, Vector())))
              .flatMap { baseTypes =>
                val messageSource = CompilationMessageSource.SourceFile(env.fileSpec, baseTypeExpr.location)

                baseTypes.baseClass.traverse_ { baseClass =>
                  osCheck.checkExtendClass[context.type, baseClass.arClass.PayloadSpec](baseClass.arClass.value)(messageSource)
                }
                  .flatMap { _ =>
                    baseTypes.baseTraits.traverse_ { baseTrait =>
                      osCheck.checkExtendTrait[context.type, baseTrait.arTrait.PayloadSpec](baseTrait.arTrait.value)(messageSource)
                    }
                  }
                  .map { _ => baseTypes }
              }
          case None =>
            IO.succeed(BaseTypeInfoClass(None, Vector()))
        })
      ).pure[Comp]
    }

    private def typeToBaseTypes
    (context: Context)
    (env: ExpressionConverter.Env[context.type, context.scopeContext.Scope])
    (t: context.typeSystem.TType)
    (location: SourceLocation)
    (acc: BaseTypeInfoClass[context.type, Id])
    : Comp[BaseTypeInfoClass[context.type, Id]] = {
      import context._
      t match {
        case t @ ClassType(_, _) =>
          if(acc.baseClass.isDefined)
            Compilation.forErrors(CompilationError.MultipleBaseClasses(CompilationMessageSource.SourceFile(env.fileSpec, location)))
          else
            acc.copy(baseClass = Some(t)).pure[Comp]

        case t @ TraitType(_, _) =>
          acc.copy(baseTraits = acc.baseTraits :+ t).pure[Comp]

        case IntersectionType(first, second) =>
          typeToBaseTypes(context)(env)(first)(location)(acc).flatMap { acc2 =>
            typeToBaseTypes(context)(env)(second)(location)(acc2)
          }

        case _ =>
          Compilation.forErrors(CompilationError.InvalidBaseType(CompilationMessageSource.SourceFile(env.fileSpec, location)))
      }
    }
  }

}
