package dev.argon.compiler.types

import zio.{IO, UIO}

sealed trait HoleId
object HoleId {
  @SuppressWarnings(Array("scalafix:Disable.effectTotal"))
  def make: UIO[HoleId] =
    IO.effectTotal { new HoleId {} }
}
