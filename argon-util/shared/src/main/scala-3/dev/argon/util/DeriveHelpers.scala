package dev.argon.util

import magnolia._

import cats.Eq

object DeriveHelpers {
  def eq[A]: Eq[A] = ???

  object EqDerivation extends Derivation[Eq] {
    def join[T](ctx: CaseClass[Eq, T]): Eq[T] = (a, b) =>
      ctx.params.forall { param =>
        param.typeclass.eqv(param.deref(a), param.deref(b))
      }

    override def split[T](ctx: SealedTrait[Eq, T]): Eq[T] = (a, b) =>
      ctx.choose(a) { sub =>
        sub.typeclass.eqv(sub.value, sub.cast(b))
      }
  }
}

