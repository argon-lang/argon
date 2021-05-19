package dev.argon.util

import cats.evidence.Is

import scala.annotation.unused

object IsHelpers {
  @SuppressWarnings(Array("scalafix:DisableSyntax.asInstanceOf"))
  def substituteBounded[L, U <: L, F[_ >: U <: L], A >: U <: L, B >: U <: L](@unused is: A Is B)(value: F[A]): F[B] =
    value.asInstanceOf[F[B]]


  @SuppressWarnings(Array("scalafix:DisableSyntax.asInstanceOf"))
  def unwrapBounded[L, U <: L, F[_ >: U <: L], A >: U <: L, B >: U <: L](is: F[A] Is F[B]): A Is B =
    is.asInstanceOf[A Is B]

  @SuppressWarnings(Array("scalafix:DisableSyntax.asInstanceOf"))
  def both[A, B](a: A)(implicit @unused is: A Is B): A with B = a.asInstanceOf[A with B]

  def absurd[A, B](a: A)(implicit is: A Is B, bothIsNothing: (A with B) Is Nothing): Nothing =
    bothIsNothing.coerce(both[A, B](a))

}
