package dev.argon.backend

import zio.*
import esexpr.ESExpr

trait ExternLoader[E] {
  def getExtern(name: String): IO[E, Option[scalaApi.ExternInfo]]
}
