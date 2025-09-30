package dev.argon.backend

import dev.argon.compiler.*
import dev.argon.vm.resource.VmIrResource
import zio.*
import java.io.IOException

trait Backend[E >: BackendException | IOException] {
  type Output

  def name: String

  val platformDataLoader: PlatformDataLoader[E]
  val codeGenerator: CodeGenerator[E, Output]

}
