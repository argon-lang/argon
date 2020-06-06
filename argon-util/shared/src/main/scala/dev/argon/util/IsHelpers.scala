package dev.argon.util

import cats.evidence.Is

object IsHelpers {
  @SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
  def substituteBounded[L, U <: L, F[_ >: U <: L], A >: U <: L, B >: U <: L](is: A Is B)(value: F[A]): F[B] =
    value.asInstanceOf[F[B]]
}
