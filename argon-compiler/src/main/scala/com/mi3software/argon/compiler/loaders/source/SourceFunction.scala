package com.mi3software.argon.compiler.loaders.source

import com.mi3software.argon.compiler._
import com.mi3software.argon.compiler.core._
import com.mi3software.argon.compiler.loaders.source.SourceSignatureCreator.ResultCreator
import com.mi3software.argon.compiler.lookup._
import com.mi3software.argon.parser
import com.mi3software.argon.util.{FileSpec, WithSource}
import scalaz._
import Scalaz._

private[compiler] object SourceFunction {

  def apply[TComp[+_] : Compilation]
  (context2: ContextComp[TComp])
  (scope: context2.Scope)
  (stmt: parser.FunctionDeclarationStmt)
  (fileSpec: FileSpec)
  (desc: FuncDescriptor)
  : ArFunc[context2.type, PayloadSpecifiers.DeclarationPayloadSpecifier] =
    new ArFunc[context2.type, PayloadSpecifiers.DeclarationPayloadSpecifier] {
      override val context: context2.type = context2

      override val descriptor: FuncDescriptor = desc

      override val effectInfo: EffectInfo = EffectInfo(stmt.purity)

      override lazy val signature: TComp[context.Signature[FunctionResultInfo]] =
        SourceSignatureCreator.fromParameters(context2)(
          ExpressionConverter.Env(descriptor, scope, fileSpec)
        )(descriptor)(stmt.parameters)(resultCreator(stmt.returnType))

      override lazy val payload: TComp[context.TFunctionImplementation] = ??? : TComp[context.TFunctionImplementation]
    }

  def resultCreator(returnTypeExpr: WithSource[parser.Expr]): ResultCreator[FunctionResultInfo] =  new ResultCreator[FunctionResultInfo] {
    override def createResult[TComp[+ _] : Compilation]
    (context: ContextComp[TComp])
    (env: ExpressionConverter.Env[context.Scope])
    : TComp[FunctionResultInfo[context.typeSystem.type]] =
      ExpressionConverter.convertTypeExpression(context)(env)(returnTypeExpr)
        .map { t => FunctionResultInfo(context.typeSystem)(t) }
  }

}
