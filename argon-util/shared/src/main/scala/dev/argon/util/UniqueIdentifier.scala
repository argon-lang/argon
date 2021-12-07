package dev.argon.util

import zio.{IO, UIO}

sealed trait UniqueIdentifier derives CanEqual
object UniqueIdentifier {

  @SuppressWarnings(Array("scalafix:Disable.effect"))
  def make: UIO[UniqueIdentifier] =
    IO.effectTotal { new UniqueIdentifier {} }

}