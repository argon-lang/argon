package dev.argon.backend.platforms.js

import dev.argon.backend.*

import dev.argon.io.*
import dev.argon.compiler.*
import zio.*
import zio.stream.*
import java.io.IOException

final class JSBackend[E >: BackendException | IOException] extends Backend[E] with JSBackendPlatformSpecific[E] {
  
  final case class JSOptions(
    externs: Seq[TextResource[E]],
  )

  final case class JSOutput(
    sourceCode: DirectoryResource[E, TextResource],
  )

  override type Options = JSOptions
  override type Output = JSOutput

  
  override def name: String = "js"

}
