package com.mi3software.argon.compiler

import com.mi3software.argon.parser.FunctionDeclarationStmt
import com.mi3software.argon.Compilation
import scalaz._

private[compiler] object SourceFunction {

  def apply[TComp[+_] : Monad : Compilation]
  (context2: ContextComp[TComp])
  (scope: Scope[context2.ContextScopeTypes])
  (stmt: FunctionDeclarationStmt)
  (desc: FuncDescriptor)
  : ArFuncWithPayload[context2.type, PayloadSpecifiers.DeclarationPayloadSpecifier] =
    new ArFuncWithPayload[context2.type, PayloadSpecifiers.DeclarationPayloadSpecifier] {
      override val context: context2.type = context2

      override val descriptor: FuncDescriptor = desc

      override val effectInfo: EffectInfo = EffectInfo(stmt.purity)

      override lazy val signature: Signature[context.typeSystem.type, FunctionResultInfo] = ??? : Signature[context.typeSystem.type, FunctionResultInfo]
      override lazy val payload: TComp[context.TFunctionImplementation] = ??? : TComp[context.TFunctionImplementation]
    }

}
