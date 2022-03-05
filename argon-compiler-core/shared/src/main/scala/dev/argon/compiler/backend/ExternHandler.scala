package dev.argon.compiler.backend

import dev.argon.compiler.*
import dev.argon.io.*
import zio.*

trait ExternHandler {
  type ExternFunction
  type ExternMethod

  def loadExternFunction(source: DiagnosticSource, name: String): UIO[ExternFunction]
  def loadExternMethod(source: DiagnosticSource, name: String): UIO[ExternMethod]
}
