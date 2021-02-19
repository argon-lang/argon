package dev.argon.backend.generic

import dev.argon.backend.ExternHandler
import dev.argon.compiler.{Comp, CompStream, DiagnosticError, DiagnosticSource}
import zio.IO

class GenericExternHandler extends ExternHandler {

  override type ExternFunction = Nothing
  override type ExternMethod = Nothing


  override def loadExternFunction(source: DiagnosticSource, name: String): Comp[Nothing] =
    IO.fail(DiagnosticError.UnknownExternImplementation(name, source))

  override def loadExternMethod(source: DiagnosticSource, name: String): Comp[Nothing] =
    IO.fail(DiagnosticError.UnknownExternImplementation(name, source))

  override def encodeExternalFunction(source: DiagnosticSource, extern: Nothing)(platform: String): CompStream[Byte] = extern

  override def encodeExternalMethod(source: DiagnosticSource, extern: Nothing)(platform: String): CompStream[Byte] = extern

}


