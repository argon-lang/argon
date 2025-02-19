package dev.argon.backend

import dev.argon.compiler.TubeName
import dev.argon.vm.resource.VmIrResource
import java.io.IOException
import zio.*

sealed abstract class CodeGenerator[E >: BackendException | IOException, Output] {
  type Options
}

object CodeGenerator {
  abstract class LibraryCodeGenerator[E >: BackendException | IOException, Output] extends CodeGenerator[E, Output] {

    def codegen(
      options: Options,
      program: VmIrResource[E],
      libraries: Map[TubeName, VmIrResource[E]],
    ): ZIO[Scope, E, Output]
  }
}
