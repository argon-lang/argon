package dev.argon.compiler.types

import dev.argon.compiler.core._
import dev.argon.compiler._
import cats._

trait ArgonTypeSystem[TContext <: Context with Singleton] extends TypeSystem[TContext] {

  override type TTypeWrapper[+A] = A

  final override def wrapType[A](a: A): A = a

  override def unwrapType[A](t: A): Option[A] = Some(t)

  final override def mapTypeWrapper[A, B](t: A)(f: A => B): B = f(t)

  override def traverseTypeWrapper[A, B, F[_] : Applicative](t: A)(f: A => F[B]): F[B] =
    f(t)

  override def wrapExprType(expr: WrapExpr): TType = expr.exprType

  override def isSubTypeWrapper[TComp[_] : Compilation](a: TType, b: TType): TComp[Option[SubTypeInfo[TType]]] =
    isSimpleSubType(a, b)

  override def universeOfExpr(expr: WrapExpr): Universe = expr.universe

  override def universeOfType(t: TType): TypeUniverse = t.universe
}
