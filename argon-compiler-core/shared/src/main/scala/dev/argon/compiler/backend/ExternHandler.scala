package dev.argon.compiler.backend

import dev.argon.compiler.*
import dev.argon.io.*
import zio.*

trait ExternHandler {
  type ExternFunction <: BinaryResource
  type ExternMethod <: BinaryResource

  def loadExternFunction(source: DiagnosticSource, name: String): UIO[ExternFunction]
  def loadExternMethod(source: DiagnosticSource, name: String): UIO[ExternMethod]
}
