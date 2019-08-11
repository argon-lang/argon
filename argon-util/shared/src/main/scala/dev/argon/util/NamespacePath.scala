package dev.argon.util

import cats._
import cats.implicits._

final case class NamespacePath(ns: Vector[String])

object NamespacePath {
  val empty: NamespacePath = NamespacePath(Vector())

  implicit val eqInstance: Eq[NamespacePath] = derived.semi.eq

}
