package dev.argon.util


final case class WithSource[+T](value: T, location: SourceLocation) {
  def map[U](f: T => U): WithSource[U] = WithSource(f(value), location)
}


object WithSource {
  def lift[A, B](f: A => B): WithSource[A] => WithSource[B] = {
    case WithSource(value, loc) => WithSource(f(value), loc)
  }

  def liftF[F[+_] : Functor, A, B](f: A => F[B]): WithSource[A] => F[WithSource[B]] = {
    case WithSource(value, loc) => f(value).map(b => WithSource(b, loc))
  }
}
