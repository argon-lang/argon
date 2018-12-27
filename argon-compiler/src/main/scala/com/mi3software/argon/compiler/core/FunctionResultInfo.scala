package com.mi3software.argon.compiler.core

import com.mi3software.argon.compiler.types._

sealed trait FunctionResultInfo[+TS <: TypeSystem] {
  val typeSystem: TS
  val returnType: typeSystem.TType
}

object FunctionResultInfo {

  def apply(ts: TypeSystem)(retType: ts.TType): FunctionResultInfo[ts.type] = new FunctionResultInfo[ts.type] {
    override val typeSystem: ts.type = ts
    override val returnType: typeSystem.TType = retType
  }

}
