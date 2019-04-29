package dev.argon.compiler.vtable

import dev.argon.compiler.core._
import scalaz._
import Scalaz._

final case class VTable[TContext <: Context with Singleton]
(
  methodMap: Map[AbsRef[TContext, ArMethod], VTableEntry[TContext]]
)

object VTable {

  implicit def vtableMonoid[TContext <: Context with Singleton](implicit entrySemigroup: Semigroup[VTableEntry[TContext]]): Monoid[VTable[TContext]] = new Monoid[VTable[TContext]] {
    override def zero: VTable[TContext] = VTable(methodMap = Map.empty)

    override def append(f1: VTable[TContext], f2: => VTable[TContext]): VTable[TContext] =
      VTable(
        methodMap = f1.methodMap |+| f2.methodMap
      )
  }

}
