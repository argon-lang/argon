package dev.argon.compiler.loaders.source

import dev.argon.compiler.core.PayloadSpecifiers._
import dev.argon.compiler._
import dev.argon.compiler.core._
import dev.argon.compiler.loaders.source.ExpressionConverter.EnvCreator
import dev.argon.compiler.loaders.source.SourceSignatureCreator.ResultCreator
import dev.argon.compiler.lookup._
import dev.argon.parser
import dev.argon.parser.TraitDeclarationStmt
import dev.argon.util.{FileID, SourceLocation, ValueCache, WithSource}
import cats.{Id => _, _}
import cats.implicits._
import cats.evidence.Is
import dev.argon.compiler.expr.ArExpr._
import dev.argon.compiler.expr.BaseTypeInfoTrait
import shapeless.{Id, Nat}
import zio.interop.catz.core._

private[compiler] object SourceTrait extends AccessModifierHelpers {

  final case class GroupedStaticStatements
  (
    staticMethods: Vector[WithSource[parser.MethodDeclarationStmt]],
  )

  final case class GroupedInstanceStatements
  (
    methods: Vector[WithSource[parser.MethodDeclarationStmt]],
  )

  def apply
  (context2: Context)
  (env: EnvCreator[context2.type])
  (stmt: TraitDeclarationStmt)
  (desc: TraitDescriptor)
  : Comp[ArTrait[context2.type, PayloadSpecifiers.DeclarationPayloadSpecifier] { val descriptor: desc.type }] = {
    import context2._
    
    for {
      sigCache <- ValueCache.make[ErrorList, context2.signatureContext.Signature[ArTrait.ResultInfo, _ <: Nat]]
      sigResultCache <- ValueCache.make[ErrorList, BaseTypeInfoTrait[context2.type, Id]]

      paramsEnvCache <- ValueCache.make[ErrorList, EnvCreator[context2.type]]

      groupedStaticCache <- ValueCache.make[ErrorList, GroupedStaticStatements]
      groupedInstCache <- ValueCache.make[ErrorList, GroupedInstanceStatements]

      methodCache <- ValueCache.make[ErrorList, Vector[MethodBinding[context2.type, DeclarationPayloadSpecifier]]]
      staticMethodCache <- ValueCache.make[ErrorList, Vector[MethodBinding[context2.type, DeclarationPayloadSpecifier]]]

    } yield new ArTrait[context2.type, PayloadSpecifiers.DeclarationPayloadSpecifier] with OpenSealedCheck {
      override val context: context2.type = context2

      import context.signatureContext.Signature

      override val contextProof: context.type Is context2.type = Is.refl

      override val descriptor: desc.type = desc
      override val fileId: FileID = env.fileSpec.fileID

      override val isSealed: Boolean = stmt.modifiers.exists {
        case WithSource(parser.SealedModifier, _) => true
        case _ => false
      }

      override lazy val signature: Comp[Signature[ArTrait.ResultInfo, _ <: Nat]] =
        sigCache.get(
          SourceSignatureCreator.fromParameters[ArTrait.ResultInfo](context2)(
            env(context)(EffectInfo.pure, descriptor)
          )(descriptor)(stmt.parameters)(resultCreator(context)(stmt.baseType, sigResultCache)(this))
        )

      private val paramsEnv: Comp[EnvCreator[context.type]] =
        paramsEnvCache.get(
          signature.map { sig =>
            env.addParameters(context)(sig.unsubstitutedParameters)
          }
        )

      private val groupedStatic =
        groupedStaticCache.get(
          stmt.body.toVector.foldLeftM(GroupedStaticStatements(Vector.empty)) {
            case (group, WithSource(stmt: parser.MethodDeclarationStmt, location)) =>
              group.copy(staticMethods = group.staticMethods :+ WithSource(stmt, location)).pure[Comp]

            case (_, WithSource(_, location)) =>
              Compilation.forErrors(CompilationError.UnexpectedStatement(CompilationMessageSource.SourceFile(env.fileSpec, location)))
          }
        )

      private val groupedInst =
        groupedInstCache.get(
          stmt.instanceBody.foldLeftM(GroupedInstanceStatements(Vector.empty)) {
            case (group, WithSource(stmt: parser.MethodDeclarationStmt, location)) =>
              group.copy(methods = group.methods :+ WithSource(stmt, location)).pure[Comp]

            case (_, WithSource(_, location)) =>
              Compilation.forErrors(CompilationError.UnexpectedStatement(CompilationMessageSource.SourceFile(env.fileSpec, location)))
          }
        )



      override val methods: Comp[Vector[MethodBinding[context2.type, DeclarationPayloadSpecifier]]] =
        methodCache.get(groupedInst.flatMap { inst =>
          inst.methods.zipWithIndex.traverse { case (method, i) =>
            parseAccessModifier(env.fileSpec, method.location, getAccessModifiers(method.value.modifiers)).flatMap { modifiers =>
              val memberName = MethodName.fromMethodNameSpecifier(method.value.name)

              paramsEnv.flatMap { env2 =>
                val desc = MethodDescriptor(descriptor, i, memberName)
                SourceMethod(context)(env2)(method.value, method.location)(desc)(ArMethod.TraitOwner(this))
                  .map(MethodBinding(memberName, i, modifiers, _))
              }
            }
          }
        })

      override val staticMethods: Comp[Vector[MethodBinding[context2.type, DeclarationPayloadSpecifier]]] =
        staticMethodCache.get(groupedStatic.flatMap { statics =>
          statics.staticMethods.zipWithIndex.traverse { case (method, i) =>
            parseAccessModifier(env.fileSpec, method.location, getAccessModifiers(method.value.modifiers)).flatMap { modifiers =>
              val memberName = MethodName.fromMethodNameSpecifier(method.value.name)

              paramsEnv.flatMap { env2 =>
                val desc = MethodDescriptor(TraitObjectDescriptor(descriptor), i, memberName)
                SourceMethod(context)(env2)(method.value, method.location)(desc)(ArMethod.TraitObjectOwner(this))
                  .map(MethodBinding(memberName, i, modifiers, _))
              }
            }
          }
        })
      override val payload: Unit = ()


    }
  }


  private def resultCreator(ctx: Context)(baseTypeExpr: Option[WithSource[parser.Expr]], cache: ValueCache[ErrorList, BaseTypeInfoTrait[ctx.type, Id]])(osCheck: OpenSealedCheck): ResultCreator.Aux[ctx.type, ArTrait.ResultInfo] = new ResultCreator[ArTrait.ResultInfo] {

    override val context: ctx.type = ctx

    override def createResult
    (env: ExpressionConverter.Env[context.type, context.scopeContext.Scope])
    : Comp[ArTrait.ResultInfo[context.type, context.typeSystem.TTypeWrapper]] =
      ArTrait.ResultInfo(
        cache.get(baseTypeExpr match {
          case Some(baseTypeExpr) =>
            ExpressionConverter.convertTypeExpression(context)(env)(baseTypeExpr)
              .flatMap(typeToBaseTypes(context)(env)(_)(baseTypeExpr.location)(BaseTypeInfoTrait[context.type, Id](Vector())))
              .flatMap { baseTypes =>
                val messageSource = CompilationMessageSource.SourceFile(env.fileSpec, baseTypeExpr.location)

                baseTypes.baseTraits.traverse_ { baseTrait =>
                  osCheck.checkExtendTrait[context.type, baseTrait.arTrait.PayloadSpec](baseTrait.arTrait.value)(messageSource)
                }
                  .map { _ => baseTypes }
              }
          case None =>
            BaseTypeInfoTrait[context.type, Id](Vector()).pure[Comp]
        })
      ).pure[Comp]

  }

  private def typeToBaseTypes
  (context: Context)
  (env: ExpressionConverter.Env[context.type, context.scopeContext.Scope])
  (t: context.typeSystem.TType)
  (location: SourceLocation)
  (acc: BaseTypeInfoTrait[context.type, Id])
  : Comp[BaseTypeInfoTrait[context.type, Id]] = {
    import context._
    t match {
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
