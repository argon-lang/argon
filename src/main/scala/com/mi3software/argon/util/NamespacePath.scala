package com.mi3software.argon.util

import scalaz._
import Scalaz._

@deriving(Equal)
final case class NamespacePath(ns: Vector[String])

object NamespacePath {
  val empty: NamespacePath = NamespacePath(Vector())
}
