package dev.argon.util.xml

final case class XMLException(cause: DOMException) extends Exception(cause.message)
