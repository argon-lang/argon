package dev.argon.util

import magnolia._

import cats.Eq
import scala.deriving.Mirror

object DeriveHelpers {
  inline def eq[A: Mirror.Of]: Eq[A] = EqDerivation.derived

  object EqDerivation extends Derivation[Eq] {
    def join[T](ctx: CaseClass[Eq, T]): Eq[T] = (a, b) =>
      ctx.params.forall { param =>
        param.typeclass.eqv(param.deref(a), param.deref(b))
      }

    override def split[T](ctx: SealedTrait[Eq, T]): Eq[T] = (a, b) =>
      a.getClass == b.getClass && ctx.choose(a) { sub =>
        sub.typeclass.eqv(sub.value, sub.cast(b))
      }
  }
}

