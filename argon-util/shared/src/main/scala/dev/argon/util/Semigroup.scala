package dev.argon.util

trait Semigroup[A] {
  def combine(x: A, y: A): A
}

object Semigroup {
  def apply[A](using sg: Semigroup[A]): Semigroup[A] = sg
}
