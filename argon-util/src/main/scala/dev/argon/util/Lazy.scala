package dev.argon.util

final class Lazy[+T](valueUncached: => T) {
  lazy val value: T = valueUncached
}

object Lazy {
  def apply[T](value: => T): Lazy[T] = new Lazy[T](value)
}
