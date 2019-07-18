package dev.argon.compiler.loaders.source

import dev.argon.compiler.core.PayloadSpecifiers._
import dev.argon.compiler._
import dev.argon.compiler.core._
import dev.argon.compiler.loaders.source.ExpressionConverter.EnvCreator
import dev.argon.compiler.loaders.source.SourceSignatureCreator.ResultCreator
import dev.argon.compiler.lookup._
import dev.argon.parser
import dev.argon.parser.TraitDeclarationStmt
import dev.argon.util.{FileID, SourceLocation, WithSource}
import cats._
import cats.implicits._
import cats.evidence.Is

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
  : context2.Comp[ArTrait[context2.type, PayloadSpecifiers.DeclarationPayloadSpecifier] { val descriptor: desc.type }] = {
    import context2._
    
    for {
      sigCache <- Compilation[Comp].createCache[context2.signatureContext.Signature[ArTrait.ResultInfo]]

      groupedStaticCache <- Compilation[Comp].createCache[GroupedStaticStatements]
      groupedInstCache <- Compilation[Comp].createCache[GroupedInstanceStatements]

      methodCache <- Compilation[Comp].createCache[Vector[MethodBinding[context2.type, DeclarationPayloadSpecifier]]]
      staticMethodCache <- Compilation[Comp].createCache[Vector[MethodBinding[context2.type, DeclarationPayloadSpecifier]]]

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

      override lazy val signature: Comp[Signature[ArTrait.ResultInfo]] =
        sigCache(
          SourceSignatureCreator.fromParameters[ArTrait.ResultInfo](context2)(
            env(context)(EffectInfo.pure, descriptor)
          )(descriptor)(stmt.parameters)(resultCreator(stmt.baseType)(this))
        )

      private val groupedStatic =
        groupedStaticCache(
          stmt.body.toVector.foldLeftM(GroupedStaticStatements(Vector.empty)) {
            case (group, WithSource(stmt: parser.MethodDeclarationStmt, location)) =>
              group.copy(staticMethods = group.staticMethods :+ WithSource(stmt, location)).pure[Comp]

            case (_, WithSource(_, location)) =>
              Compilation[Comp].forErrors(CompilationError.UnexpectedStatement(CompilationMessageSource.SourceFile(env.fileSpec, location)))
          }
        )

      private val groupedInst =
        groupedInstCache(
          stmt.instanceBody.foldLeftM(GroupedInstanceStatements(Vector.empty)) {
            case (group, WithSource(stmt: parser.MethodDeclarationStmt, location)) =>
              group.copy(methods = group.methods :+ WithSource(stmt, location)).pure[Comp]

            case (_, WithSource(_, location)) =>
              Compilation[Comp].forErrors(CompilationError.UnexpectedStatement(CompilationMessageSource.SourceFile(env.fileSpec, location)))
          }
        )



      override val methods: Comp[Vector[MethodBinding[context2.type, DeclarationPayloadSpecifier]]] =
        methodCache(groupedInst.flatMap { inst =>
          inst.methods.zipWithIndex.traverse { case (method, i) =>
            parseAccessModifier[Comp](env.fileSpec, method.location, getAccessModifiers(method.value.modifiers)).flatMap { modifiers =>
              val memberName = method.value.name match {
                case Some(name) => MemberName.Normal(name)
                case None => MemberName.Unnamed
              }

              val desc = MethodDescriptor(descriptor, i, memberName)
              SourceMethod(context)(env)(method.value, method.location)(desc)(ArMethod.TraitOwner(this))
                .map(MethodBinding(memberName, i, modifiers, _))
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

              val desc = MethodDescriptor(TraitObjectDescriptor(descriptor), i, memberName)
              SourceMethod(context)(env)(method.value, method.location)(desc)(ArMethod.TraitObjectOwner(this))
                .map(MethodBinding(memberName, i, modifiers, _))
            }
          }
        })
      override val payload: Unit = ()


    }
  }


  private def resultCreator(baseTypeExpr: Option[WithSource[parser.Expr]])(osCheck: OpenSealedCheck): ResultCreator[ArTrait.ResultInfo] = new ResultCreator[ArTrait.ResultInfo] {
    override def createResult
    (context: Context)
    (env: ExpressionConverter.Env[context.type, context.scopeContext.Scope])
    : context.Comp[ArTrait.ResultInfo[context.type, context.typeSystem.type]] = {
      import context._

      (baseTypeExpr match {
        case Some(baseTypeExpr) =>
          ExpressionConverter.convertTypeExpression(context)(env)(baseTypeExpr)
            .flatMap(typeToBaseTypes(context)(env)(_)(baseTypeExpr.location)(context.typeSystem.BaseTypeInfoTrait(Vector())))
            .flatMap { baseTypes =>
              val messageSource = CompilationMessageSource.SourceFile(env.fileSpec, baseTypeExpr.location)

              baseTypes.baseTraits.traverse_ { baseTrait =>
                osCheck.checkExtendTrait[Comp, context.type, baseTrait.arTrait.PayloadSpec](baseTrait.arTrait.value)(messageSource)
              }
                .map { _ => baseTypes }
            }
        case None =>
          context.typeSystem.BaseTypeInfoTrait(Vector()).pure[Comp]
      })
        .map { baseTypes => ArTrait.ResultInfo(context.typeSystem)(baseTypes) }
    }
  }

  private def typeToBaseTypes
  (context: Context)
  (env: ExpressionConverter.Env[context.type, context.scopeContext.Scope])
  (t: context.typeSystem.TType)
  (location: SourceLocation)
  (acc: context.typeSystem.BaseTypeInfoTrait)
  : context.Comp[context.typeSystem.BaseTypeInfoTrait] = {
    import context._
    t match {
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
