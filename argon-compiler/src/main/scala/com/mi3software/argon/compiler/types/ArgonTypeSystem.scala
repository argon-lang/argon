package com.mi3software.argon.compiler.types

import com.mi3software.argon.compiler.core._
import com.mi3software.argon.compiler._

trait ArgonTypeSystem extends TypeSystem {

  override type TTypeWrapper[A] = A

  final override def wrapType[A](a: A): A = a

  final override def mapTypeWrapper[A, B](t: A)(f: A => B): B = f(t)

  final override def isSubTypeWrapper[TComp[+ _] : Compilation, T](f: (T, T) => TComp[Boolean])(a: T, b: T): TComp[Boolean] =
    f(a, b)
}
