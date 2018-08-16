package com.mi3software.argon.compiler

import com.mi3software.argon.compiler.SourceSignatureCreator.ResultCreator
import com.mi3software.argon.parser
import com.mi3software.argon.util.{ FileSpec, WithSource }
import scalaz._
import Scalaz._
import ScopeHelpers._

private[compiler] object SourceFunction {

  def apply[TComp[+_] : Compilation]
  (context2: ContextComp[TComp])
  (expressionConverter: ExpressionConverterCombined[context2.type])
  (scope: Scope[context2.ContextScopeTypes])
  (stmt: parser.FunctionDeclarationStmt)
  (fileSpec: FileSpec)
  (desc: FuncDescriptor)
  : ArFuncWithPayload[context2.type, PayloadSpecifiers.DeclarationPayloadSpecifier] =
    new ArFuncWithPayload[context2.type, PayloadSpecifiers.DeclarationPayloadSpecifier] {
      override val context: context2.type = context2

      override val descriptor: FuncDescriptor = desc

      override val effectInfo: EffectInfo = EffectInfo(stmt.purity)

      override lazy val signature: TComp[Signature[context.typeSystem.type, FunctionResultInfo]] = {
        val env = ExpressionConvertEnvironment(descriptor, scope.convertTypes(expressionConverter.scopeTypeConverter), fileSpec)

        SourceSignatureCreator.fromParameters(context2)(expressionConverter)(env)(descriptor)(stmt.parameters)(resultCreator(stmt.returnType))
      }

      override lazy val payload: TComp[context.TFunctionImplementation] = ??? : TComp[context.TFunctionImplementation]
    }

  def resultCreator(returnTypeExpr: WithSource[parser.Expr]): ResultCreator[FunctionResultInfo] =  new ResultCreator[FunctionResultInfo] {
    override def createResult[TComp[+ _] : Compilation]
    (context: ContextComp[TComp])
    (expressionConverter: ExpressionConverterContext[context.type])
    (env: ExpressionConvertEnvironment[expressionConverter.TScopeTypes])
    : TComp[FunctionResultInfo[context.typeSystem.type]] =
      expressionConverter.convertTypeExpressionResolved(env)(returnTypeExpr)
        .map { t => FunctionResultInfo[context.typeSystem.type](t) }
  }

}
