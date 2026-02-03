package dev.argon.backend

import zio.*

trait ExternLoader[E] {
  def getExtern(name: String): IO[E, Option[scalaApi.ExternInfo]]
}
