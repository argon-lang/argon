package dev.argon.util

import cats.*
import cats.implicits.given

final case class WithLocation[+T, Pos](value: T, location: Location[Pos]) {
  def map[U](f: T => U): WithLocation[U, Pos] = WithLocation(f(value), location)
}

object WithLocation {

  def lift[A, B, Pos](f: A => B): WithLocation[A, Pos] => WithLocation[B, Pos] = {
    case WithLocation(value, loc) => WithLocation(f(value), loc)
  }

  def liftF[F[+_]: Functor, A, B, Pos](f: A => F[B]): WithLocation[A, Pos] => F[WithLocation[B, Pos]] = {
    case WithLocation(value, loc) => f(value).map(b => WithLocation(b, loc))
  }

}

type WithSource[+T] = WithLocation[T, FilePosition]
