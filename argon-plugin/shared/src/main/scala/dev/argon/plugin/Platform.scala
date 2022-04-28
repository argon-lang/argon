package dev.argon.plugin

trait Platform[E] {
  val id: String
  val name: String

  type ExternFunction
  type ExternMethod
  type ExternClassConstructor

  val externFunctionCodec: ExternCodec[E, ExternFunction]
  val externMethodCodec: ExternCodec[E, ExternMethod]
  val externClassConstructorCodec: ExternCodec[E, ExternClassConstructor]
}
