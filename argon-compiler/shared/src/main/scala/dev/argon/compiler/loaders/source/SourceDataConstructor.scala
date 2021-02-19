package dev.argon.compiler.loaders.source

import dev.argon.compiler._
import dev.argon.compiler.core.PayloadSpecifiers.DeclarationPayloadSpecifier
import dev.argon.compiler.core._
import dev.argon.compiler.loaders.source.ExpressionConverter.EnvCreator
import dev.argon.parser
import dev.argon.parser.DataConstructorDeclarationStmt
import dev.argon.util.{FileID, SourceLocation, UniqueIdentifier, ValueCache, WithSource}
import cats._
import cats.evidence.Is
import cats.implicits._
import dev.argon.compiler.loaders.StandardTypeLoaders
import dev.argon.compiler.loaders.source.SourceSignatureCreator.ResultCreator
import shapeless.Nat
import zio.IO
import zio.interop.catz.core._
import AccessModifierHelpers._

private[compiler] object SourceDataConstructor {

  final case class GroupedInstanceStatements
  (
    methods: Vector[WithSource[parser.MethodDeclarationStmt]],
    body: Vector[WithSource[parser.Stmt]],
  )

  def apply
  (context2: Context)
  (env: EnvCreator[context2.type])
  (stmt: DataConstructorDeclarationStmt)
  (ctorOwner: DataConstructorOwner[context2.type, DeclarationPayloadSpecifier])
  : Comp[DataConstructor[context2.type, DeclarationPayloadSpecifier] { val owner: ctorOwner.type }] =
    for {
      uniqId <- UniqueIdentifier.make

      sigCache <- ValueCache.make[CompilationError, context2.signatureContext.Signature[DataConstructor.ResultInfo, _ <: Nat]]

      bodyStmtCache <- ValueCache.make[CompilationError, context2.typeSystem.SimpleExpr]
      bodyEnvCache <- ValueCache.make[CompilationError, EnvCreator[context2.type]]

      methodCache <- ValueCache.make[CompilationError, Vector[MethodBinding[context2.type, DeclarationPayloadSpecifier]]]

    } yield new DataConstructor[context2.type, DeclarationPayloadSpecifier] with OpenSealedCheck {
      override val context: context2.type = context2
      import context.signatureContext.Signature

      override val contextProof: context.type Is context2.type = Is.refl


      override val id: DataConstructorId = DataConstructorId(uniqId)
      override val owner: ctorOwner.type = ctorOwner

      override def ownerModuleId: ModuleId = getDataCtorModule(this)

      override val fileId: FileID = env.fileSpec.fileID
      override val ctorMessageSource: DiagnosticSource = DiagnosticSource.SourceFile(env.fileSpec, stmt.name.location)

      private lazy val groupedInst =
        stmt.body.value.foldLeft(GroupedInstanceStatements(Vector.empty, Vector.empty)) {
          case (group, WithSource(stmt: parser.MethodDeclarationStmt, location)) =>
            group.copy(methods = group.methods :+ WithSource(stmt, location))

          case (group, stmt) =>
            group.copy(body = group.body :+ stmt)
        }

      private def localVarOwner = LocalVariableOwner.ByDataConstructor(AbsRef(this))
      private def paramVarOwner = ParameterVariableOwner.ByDataConstructor(AbsRef(this))

      override val signature: Comp[Signature[DataConstructor.ResultInfo, _ <: Nat]] =
        sigCache.get(
          SourceSignatureCreator.fromParameters[DataConstructor.ResultInfo](context2)(
            env(context)(EffectInfo.pure, id, localVarOwner)
          )(paramVarOwner)(stmt.parameters)(resultCreator(stmt.returnType)(this))
        )

      private def paramEnv = for {
        sig <- signature
      } yield env.addAccessToken(AccessToken.OfDataConstructor(AbsRef(this))).addParameters(context)(
        sig.unsubstitutedParameters
      )

      private val bodyStmt =
        bodyStmtCache.get(
          for {
            pEnv <- paramEnv

            unitType <- StandardTypeLoaders.loadUnitType(context)(
              DiagnosticSource.SourceFile(env.fileSpec, stmt.body.location)
            )(pEnv.currentModule)(pEnv.referencedModules)

            expr <- ExpressionConverter.convertStatementList(context)(pEnv(context)(EffectInfo.pure, id, localVarOwner))(unitType)(WithSource(groupedInst.body, stmt.body.location))
          } yield expr
        )

      private val bodyEnv =
        bodyEnvCache.get(
          for {
            pEnv <- paramEnv
            body <- bodyStmt
          } yield ExpressionScopeExtractor.addDeclarationsCreator(context2)(body, pEnv)
        )


      override val methods: Comp[Vector[MethodBinding[context2.type, DeclarationPayloadSpecifier]]] =
        methodCache.get(
          groupedInst.methods.zipWithIndex.traverse { case (method, i) =>
            for {
              modifiers <- parseAccessModifier(env.fileSpec, method.location, getAccessModifiers(method.value.modifiers))

              memberName = MethodName.fromMethodNameSpecifier(method.value.name)

              bEnv <- bodyEnv

              method <- SourceMethod(context)(bEnv)(method.value, method.location)(MethodOwner.ByDataCtor(this))
            } yield MethodBinding(modifiers, method)
          }
        )


      override lazy val payload: Comp[context.TDataConstructorImplementation] = for {
        stmt <- bodyStmt
      } yield context.createDataConstructorImplementation(stmt)



      private def resultCreator(baseTypeExpr: WithSource[parser.Expr])(osCheck: OpenSealedCheck): ResultCreator.Aux[context.type, DataConstructor.ResultInfo] = new ResultCreator[DataConstructor.ResultInfo] {

        override val context: context2.type = context2

        override def createResult
        (env: ExpressionConverter.Env[context.type, context.scopeContext.Scope])
        : Comp[DataConstructor.ResultInfo[context.type, context.typeSystem.TTypeWrapper]] =
          ExpressionConverter.convertTypeExpression(context)(env)(baseTypeExpr)
            .flatMap(typeToBaseTypes(context)(env)(_)(baseTypeExpr.location))
            .flatMap { baseTrait =>
              val messageSource = DiagnosticSource.SourceFile(env.fileSpec, baseTypeExpr.location)

              osCheck.checkExtendTrait[context.type, baseTrait.arTrait.PayloadSpec](baseTrait.arTrait.value)(messageSource)
                .map { _ => DataConstructor.ResultInfo(baseTrait) }
            }

        private def typeToBaseTypes
        (context: Context)
        (env: ExpressionConverter.Env[context.type, context.scopeContext.Scope])
        (t: context.typeSystem.TType)
        (location: SourceLocation)
        : Comp[context.typeSystem.TTraitType] =
          t match {
            case t: context.typeSystem.TTraitType =>
              IO.succeed(t)

            case _ =>
              Compilation.forErrors(DiagnosticError.InvalidBaseType(DiagnosticSource.SourceFile(env.fileSpec, location)))
          }
      }

    }

}
