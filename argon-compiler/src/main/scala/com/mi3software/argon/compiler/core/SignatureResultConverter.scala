package com.mi3software.argon.compiler.core

import com.mi3software.argon.compiler.types.{TypeSystem, TypeSystemConverter}

trait SignatureResultConverter[TResult[TContext <: Context with Singleton, _ <: TypeSystem[TContext] with Singleton]] {
  def convertTypeSystem
  (context: Context)
  (ts1: TypeSystem[context.type])
  (ts2: TypeSystem[context.type])
  (converter: TypeSystemConverter[context.type, ts1.type, ts2.type])
  (result: TResult[context.type, ts1.type])
  : TResult[context.type, ts2.type]
}
