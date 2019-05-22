package dev.argon.compiler.loaders.source

import dev.argon.compiler._
import dev.argon.compiler.core._
import dev.argon.compiler.loaders.source.SourceSignatureCreator.ResultCreator
import dev.argon.compiler.lookup._
import dev.argon.parser
import dev.argon.util.{FileID, FileSpec, WithSource}
import cats._
import cats.implicits._
import dev.argon.compiler.loaders.source.ExpressionConverter.EnvCreator

private[compiler] object SourceFunction {

  def apply[TComp[+_] : Compilation]
  (context2: ContextComp[TComp])
  (env: EnvCreator[context2.type])
  (stmt: parser.FunctionDeclarationStmt)
  (desc: FuncDescriptor)
  : ArFunc[context2.type, PayloadSpecifiers.DeclarationPayloadSpecifier] { val descriptor: desc.type } =
    new ArFunc[context2.type, PayloadSpecifiers.DeclarationPayloadSpecifier] {
      override val context: context2.type = context2

      import context.scopeContext.ScopeExtensions

      override val descriptor: desc.type = desc
      override val fileId: FileID = env.fileSpec.fileID

      override val effectInfo: EffectInfo = EffectInfo(stmt.purity)

      override lazy val signature: TComp[context.signatureContext.Signature[FunctionResultInfo]] =
        SourceSignatureCreator.fromParameters[TComp, FunctionResultInfo](context2)(
          env(context)(effectInfo, descriptor)
        )(descriptor)(stmt.parameters)(resultCreator(stmt.returnType))

      override lazy val payload: TComp[context.TFunctionImplementation] = stmt.body match {
        case WithSource(Vector(WithSource(parser.ExternExpr(specifier), location)), _) =>
          context.createExternFunctionImplementation(specifier, CompilationMessageSource.SourceFile(env.fileSpec, location))

        case _ =>
          for {
            sig <- signature
            env2 = env(context)(effectInfo, descriptor)
            env3 = env2.copy(scope = env2.scope.addVariables(
              sig.unsubstitutedParameters.flatMap(_.tupleVars)
            ))
            expr <- ExpressionConverter.convertStatementList(context)(env3)(sig.unsubstitutedResult.returnType)(stmt.body)
          } yield context.createExprFunctionImplementation(expr)
      }
    }

  def resultCreator(returnTypeExpr: WithSource[parser.Expr]): ResultCreator[FunctionResultInfo] =  new ResultCreator[FunctionResultInfo] {
    override def createResult[TComp[+ _] : Compilation]
    (context: ContextComp[TComp])
    (env: ExpressionConverter.Env[context.type, context.scopeContext.Scope])
    : TComp[FunctionResultInfo[context.type, context.typeSystem.type]] =
      ExpressionConverter.convertTypeExpression(context)(env)(returnTypeExpr)
        .map { t => FunctionResultInfo(context.typeSystem)(t) }
  }

}
