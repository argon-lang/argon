package dev.argon.util

import cats.evidence.Is

import scala.annotation.nowarn

object IsHelpers {
  @SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
  @nowarn("cat=unused-params")
  def substituteBounded[L, U <: L, F[_ >: U <: L], A >: U <: L, B >: U <: L](is: A Is B)(value: F[A]): F[B] =
    value.asInstanceOf[F[B]]
}
