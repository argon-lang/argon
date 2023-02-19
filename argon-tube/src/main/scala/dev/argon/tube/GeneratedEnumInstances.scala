package dev.argon.tube

import scalapb.{GeneratedEnum, GeneratedEnumCompanion}

object GeneratedEnumInstances {

  given [A <: GeneratedEnum](using GeneratedEnumCompanion[A]): CanEqual[A, A] = CanEqual.derived


}
