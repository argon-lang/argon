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

  implicit val sigResConverterInstance: SignatureResultConverter[FunctionResultInfo] = new SignatureResultConverter[FunctionResultInfo] {
    override def convertTypeSystem
    (context: Context)
    (ts1: TypeSystem[context.type])
    (ts2: TypeSystem[context.type])
    (converter: TypeSystemConverter[context.type, ts1.type, ts2.type])
    (result: FunctionResultInfo[context.type, ts1.type])
    : FunctionResultInfo[context.type, ts2.type] =
      FunctionResultInfo(ts2)(
        TypeSystem.convertTypeSystem(context)(ts1)(ts2)(converter)(result.returnType)
      )
  }

}
