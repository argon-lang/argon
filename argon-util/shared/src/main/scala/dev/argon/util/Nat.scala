package dev.argon.util

import cats.evidence.Is

sealed trait Nat
final case class Succ[P <: Nat]() extends Nat
sealed trait Zero extends Nat
case object Zero extends Zero

object Nat {

  sealed trait ToInt[N <: Nat] {
    val value: Int
  }

  implicit val toIntZero: ToInt[Zero] = new ToInt[Zero] {
    override val value: Int = 0
  }

  implicit def toIntSucc[P <: Nat](implicit prevToInt: ToInt[P]): ToInt[Succ[P]] = {
    val n = prevToInt.value + 1
    new ToInt[Succ[P]] {
      override val value: Int = n
    }
  }

  @SuppressWarnings(Array("scalafix:DisableSyntax.asInstanceOf"))
  implicit def succIsNotZero[P <: Nat]: (Zero with Succ[P]) Is Nothing =
    Is.refl[Zero with Succ[P]].asInstanceOf[(Zero with Succ[P]) Is Nothing]

}



