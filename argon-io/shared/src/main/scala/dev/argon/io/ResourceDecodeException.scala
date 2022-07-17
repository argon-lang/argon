package dev.argon.io

final class ResourceDecodeException(message: String, cause: Throwable) extends Exception(message, cause) {
  def this(message: String) = this(message, null)
}
