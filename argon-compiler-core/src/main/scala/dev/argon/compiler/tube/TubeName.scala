package dev.argon.compiler.tube

import dev.argon.util.*

final case class TubeName(name: NonEmptyList[String]) derives CanEqual {
  override def toString: String = name.toList.mkString(".")
}
