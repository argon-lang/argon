package dev.argon.io

final case class ResourceDecodeException(message: String, cause: Throwable) extends Exception(message, cause)
