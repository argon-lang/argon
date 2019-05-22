package dev.argon.compiler.vtable

import dev.argon.compiler.core._
import cats._
import cats.implicits._

final case class VTable[TContext <: Context with Singleton]
(
  methodMap: Map[AbsRef[TContext, ArMethod], VTableEntry[TContext]]
)

object VTable {

  implicit def vtableMonoid[TContext <: Context with Singleton](implicit entrySemigroup: Semigroup[VTableEntry[TContext]]): Monoid[VTable[TContext]] = new Monoid[VTable[TContext]] {


    override def empty: VTable[TContext] = VTable(methodMap = Map.empty)

    override def combine(x: VTable[TContext], y: VTable[TContext]): VTable[TContext] =
      VTable(
        methodMap = x.methodMap |+| y.methodMap
      )
  }

}
