package dev.argon.compiler.loaders.source

import dev.argon.compiler._
import dev.argon.compiler.core._
import dev.argon.compiler.loaders.source.SourceSignatureCreator.ResultCreator
import dev.argon.parser
import dev.argon.util.{FileID, Id, Nat, UniqueIdentifier, WithSource}
import dev.argon.compiler.core.PayloadSpecifiers.DeclarationPayloadSpecifier
import dev.argon.compiler.loaders.source.ExpressionConverter.EnvCreator

private[compiler] object SourceFunction {

  def apply
  (context2: Context)
  (env: EnvCreator[context2.type])
  (stmt: parser.FunctionDeclarationStmt)
  (funcOwner: FunctionOwner[context2.type, DeclarationPayloadSpecifier])
  : Comp[ArFunc[context2.type, DeclarationPayloadSpecifier] { val owner: funcOwner.type }] = for {
    uniqId <- UniqueIdentifier.make
  } yield new ArFunc[context2.type, DeclarationPayloadSpecifier] {
    override val context: context2.type = context2

    import context.scopeContext.ScopeExtensions


    override val id: FunctionId = FunctionId(uniqId)
    override val owner: funcOwner.type = funcOwner
    override val fileId: FileID = env.fileSpec.fileID

    override val effectInfo: EffectInfo = EffectInfo(stmt.purity)

    private def localVarOwner = LocalVariableOwner.ByFunction(AbsRef(this))
    private def paramVarOwner = ParameterVariableOwner.ByFunction(AbsRef(this))

    override lazy val signature: Comp[context.signatureContext.Signature[FunctionResultInfo, _ <: Nat]] =
      SourceSignatureCreator.fromParameters[FunctionResultInfo](context2)(
        env(context)(effectInfo, id, localVarOwner)
      )(paramVarOwner)(stmt.parameters)(resultCreator(stmt.returnType))

    override lazy val payload: Comp[context.TFunctionImplementation] = stmt.body match {
      case WithSource(parser.ExternExpr(specifier), location) =>
        context.createExternFunctionImplementation(specifier, DiagnosticSource.SourceFile(env.fileSpec, location))

      case _ =>
        for {
          sig <- signature
          env2 = env(context)(effectInfo, id, localVarOwner)
          env3 = env2.copy(scope = env2.scope.addParameters(
            sig.unsubstitutedParameters.toVector
          ))
          expr <- ExpressionConverter.convertExpression(context)(env3)(sig.unsubstitutedResult.returnType)(stmt.body)
        } yield context.createExprFunctionImplementation(expr)
    }


    private def resultCreator(returnTypeExpr: WithSource[parser.Expr]): ResultCreator.Aux[context2.type, FunctionResultInfo] =  new ResultCreator[FunctionResultInfo] {

      override val context: context2.type = context2

      override def createResult
      (env: ExpressionConverter.Env[context.type, context.scopeContext.Scope])
      : Comp[FunctionResultInfo[context.type, context.typeSystem.TTypeWrapper]] =
        ExpressionConverter.convertTypeExpression(context)(env)(returnTypeExpr)
          .map { t => FunctionResultInfo[context.type, Id](t) }
    }
  }

}
