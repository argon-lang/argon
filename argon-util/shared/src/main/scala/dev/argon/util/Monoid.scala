package dev.argon.util

trait Monoid[A] extends Semigroup[A] {
  def identity: A
  def combineAll[C[+_]: Traverse](values: C[A]): A =
    values.foldLeft(identity)(combine)
}

object Monoid {
  def apply[A](using m: Monoid[A]): Monoid[A] = m
}

extension [A: Monoid](x: IterableOnce[A])
  def foldMonoid: A =
    x.iterator.fold(Monoid[A].identity)(Monoid[A].combine)
end extension

