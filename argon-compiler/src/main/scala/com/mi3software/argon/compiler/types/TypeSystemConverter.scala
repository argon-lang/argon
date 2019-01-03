package com.mi3software.argon.compiler.types

import com.mi3software.argon.compiler.Compilation
import com.mi3software.argon.compiler.core.Context
import scalaz.Scalaz._

trait TypeSystemConverter[TContext <: Context with Singleton, TS1 <: TypeSystem[TContext], TS2 <: TypeSystem[TContext]] {
  def convertType[A](ts1: TS1)(ts2: TS2)(fromSimpleType: ts2.SimpleType => A)(t: ts1.TTypeWrapper[A]): ts2.TTypeWrapper[A]
  def convertUniverse(ts1: TS1)(ts2: TS2)(universe: ts1.TUniverse): ts2.TUniverse
  def convertTypeUniverse(ts1: TS1)(ts2: TS2)(universe: ts1.TTypeUniverse): ts2.TTypeUniverse
}

