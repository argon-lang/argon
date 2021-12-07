package dev.argon.util

final class Lazy[+T](valueUncached: => T):
  lazy val value: T = valueUncached
end Lazy

object Lazy:
  def apply[T](value: => T): Lazy[T] = new Lazy[T](value)
end Lazy
