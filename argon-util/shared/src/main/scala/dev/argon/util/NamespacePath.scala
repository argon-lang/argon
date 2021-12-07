package dev.argon.util

final case class NamespacePath(ns: Vector[String]) derives CanEqual

object NamespacePath {
  val empty: NamespacePath = NamespacePath(Vector())
}
