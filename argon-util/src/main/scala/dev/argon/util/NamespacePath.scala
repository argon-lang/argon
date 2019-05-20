package dev.argon.util

import cats._
import cats.implicits._
import scalaz.deriving
import scalaz.std.string._
import scalaz.std.vector._

@deriving(scalaz.Equal)
final case class NamespacePath(ns: Vector[String])

object NamespacePath {
  val empty: NamespacePath = NamespacePath(Vector())

  implicit val eqInstance: Eq[NamespacePath] = derived.semi.eq

}
