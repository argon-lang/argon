package dev.argon.util

sealed trait Nat
final case class Succ[P <: Nat]() extends Nat
sealed trait Zero extends Nat
case object Zero extends Zero

object Nat {

  sealed trait ToInt[N <: Nat] {
    val value: Int
  }

  given ToInt[Zero] with
    override val value: Int = 0
  end given

  given [P <: Nat](using prevToInt: ToInt[P]): ToInt[Succ[P]] with
    override val value: Int = prevToInt.value + 1
  end given

  @SuppressWarnings(Array("scalafix:DisableSyntax.asInstanceOf"))
  given succIsNotZero[P <: Nat]: Is[Zero & Succ[P], Nothing] =
    Is.refl[Zero & Succ[P]].asInstanceOf[(Zero & Succ[P]) Is Nothing]

    

}



