package com.mi3software.argon.compiler.core

import com.mi3software.argon.compiler.types._

sealed trait FunctionResultInfo[TContext <: Context with Singleton, TS <: TypeSystem[TContext] with Singleton] {
  val typeSystem: TS
  val returnType: typeSystem.TType
}

object FunctionResultInfo {

  def apply[TContext <: Context with Singleton](ts: TypeSystem[TContext])(retType: ts.TType): FunctionResultInfo[TContext, ts.type] = new FunctionResultInfo[TContext, ts.type] {
    override val typeSystem: ts.type = ts
    override val returnType: typeSystem.TType = retType
  }

}
