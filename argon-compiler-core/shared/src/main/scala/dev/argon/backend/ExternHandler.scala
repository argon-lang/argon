package dev.argon.backend

import dev.argon.compiler.{Comp, CompStream, DiagnosticSource}

trait ExternHandler {
  type ExternFunction
  type ExternMethod

  def loadExternFunction(source: DiagnosticSource, name: String): Comp[ExternFunction]
  def loadExternMethod(source: DiagnosticSource, name: String): Comp[ExternMethod]

  def encodeExternalFunction(source: DiagnosticSource, extern: ExternFunction)(platform: String): CompStream[Byte]
  def encodeExternalMethod(source: DiagnosticSource, extern: ExternMethod)(platform: String): CompStream[Byte]
}
