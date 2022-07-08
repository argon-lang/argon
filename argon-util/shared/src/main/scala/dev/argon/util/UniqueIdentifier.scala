package dev.argon.util

import zio.*

sealed trait UniqueIdentifier derives CanEqual

object UniqueIdentifier {

  @SuppressWarnings(Array("scalafix:Disable.effect"))
  def make: UIO[UniqueIdentifier] = ZIO.succeed { new UniqueIdentifier {} }

}
