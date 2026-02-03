package dev.argon.backend

import java.io.IOException

trait Backend[E >: BackendException | IOException] {
  type Output

  def name: String

  val platformDataLoader: PlatformDataLoader[E]
  val codeGenerator: CodeGenerator[E, Output]

}
