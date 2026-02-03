package dev.argon.backend

import dev.argon.backend.options.{OptionParser, OutputProvider}
import dev.argon.vm.resource.VmIrResource

import java.io.IOException
import zio.*

sealed abstract class CodeGenerator[E >: BackendException | IOException, Output] {
  type Options
  def optionParser: OptionParser[E, Options]
  def outputProvider: OutputProvider[E, Output]
}

object CodeGenerator {
  abstract class LibraryCodeGenerator[E >: BackendException | IOException, Output] extends CodeGenerator[E, Output] {

    def codegen(
      options: Options,
      program: VmIrResource[E],
      libraries: Seq[VmIrResource[E]],
    ): ZIO[Scope, E, Output]
  }
  
}
