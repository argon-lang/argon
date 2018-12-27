package com.mi3software.argon.compiler.types

import com.mi3software.argon.compiler.core._
import com.mi3software.argon.compiler._

final class ArgonTypeSystem[TContext2 <: Context](val context: TContext2) extends TypeSystem {

  override type TTypeWrapper[A] = A

  override def fromSimpleType(typeBase: SimpleType): SimpleType =
    typeBase

  override def isSubTypeWrapper[TComp[+ _] : Compilation, T](f: (T, T) => TComp[Boolean])(a: T, b: T): TComp[Boolean] =
    f(a, b)

}
