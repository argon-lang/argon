package dev.argon.util

import cats.derived.MkEq
import cats.{Eq, derived}

object DeriveHelpers {
  def eq[A](implicit ev: MkEq[A]): Eq[A] = derived.semiauto.eq
}
