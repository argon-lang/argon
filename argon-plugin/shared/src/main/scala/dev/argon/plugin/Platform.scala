package dev.argon.plugin

trait Platform {
  val id: String
  val name: String

  type ExternFunction
  type ExternMethod
}
