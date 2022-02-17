package dev.argon.compiler.tube

import dev.argon.util.NonEmptyList

final case class TubeName(name: NonEmptyList[String]) derives CanEqual
