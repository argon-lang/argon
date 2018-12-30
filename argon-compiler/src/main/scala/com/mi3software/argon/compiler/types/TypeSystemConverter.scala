package com.mi3software.argon.compiler.types

import com.mi3software.argon.compiler.Compilation
import com.mi3software.argon.compiler.core.Context
import scalaz.Scalaz._

trait TypeSystemConverter[TContext <: Context with Singleton, TS1 <: TypeSystem[TContext], TS2 <: TypeSystem[TContext]] {
  def convertType(ts1: TS1)(ts2: TS2)(t: ts1.TTypeWrapper[ts2.SimpleType]): ts2.TTypeWrapper[ts2.SimpleType]
}

