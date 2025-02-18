package dev.argon.backend

import dev.argon.compiler.*
import dev.argon.vm.resource.VmIrResource
import zio.*
import java.io.IOException

trait Backend[E >: BackendException | IOException] {
  type Options
  type Output

  def name: String

  def codegen(
    options: Options,
    program: VmIrResource[E],
    libraries: Map[TubeName, VmIrResource[E]],
  ): ZIO[Scope, E, Output]

}
