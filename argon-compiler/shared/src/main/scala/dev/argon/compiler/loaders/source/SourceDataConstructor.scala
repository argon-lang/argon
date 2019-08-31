package dev.argon.compiler.loaders.source

import dev.argon.compiler._
import dev.argon.compiler.core.PayloadSpecifiers.DeclarationPayloadSpecifier
import dev.argon.compiler.core._
import dev.argon.compiler.loaders.source.ExpressionConverter.EnvCreator
import dev.argon.parser
import dev.argon.parser.DataConstructorDeclarationStmt
import dev.argon.util.{FileID, SourceLocation, WithSource}
import cats._
import cats.evidence.Is
import cats.implicits._
import dev.argon.compiler.loaders.StandardTypeLoaders
import dev.argon.compiler.loaders.source.SourceSignatureCreator.ResultCreator
import shapeless.Nat

private[compiler] object SourceDataConstructor extends AccessModifierHelpers {

  final case class GroupedInstanceStatements
  (
    methods: Vector[WithSource[parser.MethodDeclarationStmt]],
    body: Vector[WithSource[parser.Stmt]],
  )

  def apply
  (context2: Context)
  (env: EnvCreator[context2.type])
  (stmt: DataConstructorDeclarationStmt)
  (desc: DataConstructorDescriptor)
  : context2.Comp[DataConstructor[context2.type, PayloadSpecifiers.DeclarationPayloadSpecifier] { val descriptor: desc.type }] = {
    import context2._

    for {
      sigCache <- Compilation[Comp].createCache[context2.signatureContext.Signature[DataConstructor.ResultInfo, _ <: Nat]]

      bodyStmtCache <- Compilation[Comp].createCache[context2.typeSystem.ArExpr]
      bodyEnvCache <- Compilation[Comp].createCache[EnvCreator[context2.type]]

      methodCache <- Compilation[Comp].createCache[Vector[MethodBinding[context2.type, DeclarationPayloadSpecifier]]]

    } yield new DataConstructor[context2.type, DeclarationPayloadSpecifier] with OpenSealedCheck {
      override val context: context2.type = context2
      import context.signatureContext.Signature

      override val contextProof: context.type Is context2.type = Is.refl

      override val descriptor: desc.type = desc
      override val fileId: FileID = env.fileSpec.fileID
      override val ctorMessageSource: CompilationMessageSource = CompilationMessageSource.SourceFile(env.fileSpec, stmt.name.location)

      private lazy val groupedInst =
        stmt.body.value.foldLeft(GroupedInstanceStatements(Vector.empty, Vector.empty)) {
          case (group, WithSource(stmt: parser.MethodDeclarationStmt, location)) =>
            group.copy(methods = group.methods :+ WithSource(stmt, location))

          case (group, stmt) =>
            group.copy(body = group.body :+ stmt)
        }

      override val signature: Comp[Signature[DataConstructor.ResultInfo, _ <: Nat]] =
        sigCache(
          SourceSignatureCreator.fromParameters[DataConstructor.ResultInfo](context2)(
            env(context)(EffectInfo.pure, descriptor)
          )(descriptor)(stmt.parameters)(resultCreator(stmt.returnType)(this))
        )

      private def paramEnv = for {
        sig <- signature
      } yield env.addParameters(context)(
        sig.unsubstitutedParameters
      )

      private val bodyStmt =
        bodyStmtCache(
          for {
            pEnv <- paramEnv

            unitType <- StandardTypeLoaders.loadUnitType(context)(
              CompilationMessageSource.SourceFile(env.fileSpec, stmt.body.location)
            )(pEnv.currentModule)(pEnv.referencedModules)

            expr <- ExpressionConverter.convertStatementList(context)(pEnv(context)(EffectInfo.pure, descriptor))(unitType)(WithSource(groupedInst.body, stmt.body.location))
          } yield expr
        )

      private val bodyEnv =
        bodyEnvCache(
          for {
            pEnv <- paramEnv
            body <- bodyStmt
          } yield ExpressionScopeExtractor.addDeclarationsCreator(context2)(body, pEnv)
        )


      override val methods: Comp[Vector[MethodBinding[context2.type, DeclarationPayloadSpecifier]]] =
        methodCache(
          groupedInst.methods.zipWithIndex.traverse { case (method, i) =>
            for {
              modifiers <- parseAccessModifier[Comp](env.fileSpec, method.location, getAccessModifiers(method.value.modifiers))

              memberName = MethodName.fromMethodNameSpecifier(method.value.name)

              desc = MethodDescriptor(descriptor, i, memberName)

              bEnv <- bodyEnv

              method <- SourceMethod(context)(bEnv)(method.value, method.location)(desc)(ArMethod.DataCtorOwner(this))
            } yield MethodBinding(memberName, i, modifiers, method)
          }
        )


      override lazy val payload: Comp[context.TDataConstructorImplementation] = for {
        stmt <- bodyStmt
      } yield context.createDataConstructorImplementation(stmt)
    }
  }

  private def resultCreator(baseTypeExpr: WithSource[parser.Expr])(osCheck: OpenSealedCheck): ResultCreator[DataConstructor.ResultInfo] = new ResultCreator[DataConstructor.ResultInfo] {
    override def createResult
    (context: Context)
    (env: ExpressionConverter.Env[context.type, context.scopeContext.Scope])
    : context.Comp[DataConstructor.ResultInfo[context.type, context.typeSystem.type]] = {
      import context._

      ExpressionConverter.convertTypeExpression(context)(env)(baseTypeExpr)
        .flatMap(typeToBaseTypes(context)(env)(_)(baseTypeExpr.location))
        .flatMap { baseTrait =>
          val messageSource = CompilationMessageSource.SourceFile(env.fileSpec, baseTypeExpr.location)

          osCheck.checkExtendTrait[Comp, context.type, baseTrait.arTrait.PayloadSpec](baseTrait.arTrait.value)(messageSource)
            .map { _ => DataConstructor.ResultInfo(context.typeSystem)(baseTrait) }
        }
    }

    private def typeToBaseTypes
    (context: Context)
    (env: ExpressionConverter.Env[context.type, context.scopeContext.Scope])
    (t: context.typeSystem.TType)
    (location: SourceLocation)
    : context.Comp[context.typeSystem.TraitType] = {
      import context._
      t match {
        case t: context.typeSystem.TraitType =>
          t.pure[Comp]

        case _ =>
          Compilation[Comp].forErrors(CompilationError.InvalidBaseType(CompilationMessageSource.SourceFile(env.fileSpec, location)))
      }
    }
  }

}
