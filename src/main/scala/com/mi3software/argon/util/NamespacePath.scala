package com.mi3software.argon.util

final case class NamespacePath(ns: Vector[String])

object NamespacePath {
  val empty: NamespacePath = NamespacePath(Vector())
}
