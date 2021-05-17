package dev.argon.util

import cats.Eq
import zio.{IO, UIO}

sealed trait UniqueIdentifier
object UniqueIdentifier {

  @SuppressWarnings(Array("scalafix:Disable.effect"))
  def make: UIO[UniqueIdentifier] =
    IO.effectTotal { new UniqueIdentifier {} }

  implicit val eqInstance: Eq[UniqueIdentifier] = new Eq[UniqueIdentifier] {
    @SuppressWarnings(Array("scalafix:Disable.eq"))
    override def eqv(x: UniqueIdentifier, y: UniqueIdentifier): Boolean = x eq y
  }

}