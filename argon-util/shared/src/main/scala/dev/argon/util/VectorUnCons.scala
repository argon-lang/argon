package dev.argon.util

object VectorUnCons {
  sealed trait Result[+A]
  final case class NonEmpty[+A](head: A, tail: Vector[A]) extends Result[A]
  case object Empty extends Result[Nothing]

  def uncons[A](arg: Vector[A]): Result[A] =
    arg.headOption.fold[Result[A]](Empty) { head => NonEmpty(head, arg.drop(1)) }

  def unapply[A](arg: Vector[A]): Some[Result[A]] =
    Some(uncons(arg))

  object Rev {
    sealed trait Result[+A]
    final case class NonEmpty[+A](init: Vector[A], last: A) extends Result[A]
    case object Empty extends Result[Nothing]


    def unapply[A](arg: Vector[A]): Some[Result[A]] =
      Some(arg.lastOption.fold[Result[A]](Empty) { last => NonEmpty(arg.take(arg.size - 1), last) })
  }
}
