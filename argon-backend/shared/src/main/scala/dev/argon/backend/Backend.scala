package dev.argon.backend

import dev.argon.compiler.*
import dev.argon.vm.resource.VmIrResource
import zio.*
import java.io.IOException

trait Backend {
  type Options[+E]
  type Output[+E]

  def name: String

  def codegen[E >: BackendException | IOException](
    options: Options[E],
    program: VmIrResource[E],
    libraries: Map[TubeName, VmIrResource[E]],
  ): ZIO[Scope, E, Output[E]]

}
