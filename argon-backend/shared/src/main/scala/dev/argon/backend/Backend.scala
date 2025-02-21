package dev.argon.backend

import dev.argon.compiler.*
import dev.argon.vm.resource.VmIrResource
import zio.*
import java.io.IOException

trait Backend[E >: BackendException | IOException] {
  type Output

  def name: String

  val codeGenerator: CodeGenerator[E, Output]
  
  def testExecutor: ZIO[Scope, E, Option[TestExecutor[E, Output]]]

}
