package dev.argon.compiler.backend

import dev.argon.compiler._
import dev.argon.io._
import zio.*

trait ExternHandler {
  type ExternFunction <: BinaryResource
  type ExternMethod <: BinaryResource

  def loadExternFunction(source: DiagnosticSource, name: String): UIO[ExternFunction]
  def loadExternMethod(source: DiagnosticSource, name: String): UIO[ExternMethod]
}
