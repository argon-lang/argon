package dev.argon.compiler.types

import cats._
import cats.implicits._

sealed trait HoleType[+T]
final case class HoleTypeType[+T](t: T) extends HoleType[T]
final case class HoleTypeHole[+T](id: HoleId) extends HoleType[T]


object HoleType {
  implicit val holeTypeInstances: Traverse[HoleType] with Monad[HoleType] = new Traverse[HoleType] with Monad[HoleType] {
    override def traverse[G[_], A, B](fa: HoleType[A])(f: A => G[B])(implicit ev: Applicative[G]): G[HoleType[B]] =
      fa match {
        case HoleTypeType(t) => f(t).map(HoleTypeType.apply)
        case HoleTypeHole(id) => ev.pure(HoleTypeHole(id))
      }

    override def flatMap[A, B](fa: HoleType[A])(f: A => HoleType[B]): HoleType[B] =
      fa match {
        case HoleTypeType(t) => f(t)
        case HoleTypeHole(id) => HoleTypeHole(id)
      }

    override def tailRecM[A, B](a: A)(f: A => HoleType[Either[A, B]]): HoleType[B] =
      f(a) match {
        case HoleTypeType(Right(b)) => HoleTypeType(b)
        case HoleTypeType(Left(a)) => tailRecM(a)(f)
        case HoleTypeHole(id) => HoleTypeHole(id)
      }

    override def foldLeft[A, B](fa: HoleType[A], b: B)(f: (B, A) => B): B =
      fa match {
        case HoleTypeType(t) => f(b, t)
        case HoleTypeHole(_) => b
      }

    override def foldRight[A, B](fa: HoleType[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
      fa match {
        case HoleTypeType(t) => f(t, lb)
        case HoleTypeHole(_) => lb
      }

    override def pure[A](x: A): HoleType[A] = HoleTypeType(x)
  }

}