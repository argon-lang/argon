package dev.argon.plugin

trait Platform[R, E] {
  val id: String
  val name: String

  type ExternFunction
  type ExternMethod
  type ExternClassConstructor

  val externFunctionCodec: ExternCodec[R, E, ExternFunction]
  val externMethodCodec: ExternCodec[R, E, ExternMethod]
  val externClassConstructorCodec: ExternCodec[R, E, ExternClassConstructor]
}
