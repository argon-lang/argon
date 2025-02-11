package dev.argon.backend.platforms.js

import dev.argon.backend.*

import dev.argon.io.TextResource
import dev.argon.io.DirectoryResource
import dev.argon.vm.resource.VmIrResourceContext
import dev.argon.compiler.*

object JSBackend extends Backend with JSBackendPlatformSpecific {
  
  final case class JSOptions[E](
    externs: Seq[TextResource[E]],
  )

  final case class JSOutput[E](
    sourceCode: DirectoryResource[E, TextResource],
  )

  override type Options[E] = JSOptions[E]
  override type Output[E] = JSOutput[E]

  
  override def name: String = "js"
  

  override def outputSink(context: BackendContext)(output: Output[context.Error]): context.Comp[Unit] = ???

  
  override def testExecutor: Option[TestExecutor[Output]] = None

}
