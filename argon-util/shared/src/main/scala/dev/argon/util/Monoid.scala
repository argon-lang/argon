package dev.argon.util

trait Monoid[A] extends Semigroup[A] {
  def identity: A
}

object Monoid {
  def apply[A](using m: Monoid[A]): Monoid[A] = m
}

extension [A: Monoid](x: IterableOnce[A])
  def foldMonoid: A =
    x.fold(Monoid[A].identity)(Monoid[A].combine)
end extension

