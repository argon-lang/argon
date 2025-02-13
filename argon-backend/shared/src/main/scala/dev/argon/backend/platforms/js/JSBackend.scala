package dev.argon.backend.platforms.js

import dev.argon.backend.*

import dev.argon.io.*
import dev.argon.compiler.*
import zio.*
import zio.stream.*

final class JSBackend extends Backend with JSBackendPlatformSpecific {
  
  final case class JSOptions[+E](
    externs: Seq[TextResource[E]],
  )

  final case class JSOutput[+E](
    sourceCode: DirectoryResource[E, TextResource],
  )

  override type Options[+E] = JSOptions[E]
  override type Output[+E] = JSOutput[E]

  
  override def name: String = "js"

}
