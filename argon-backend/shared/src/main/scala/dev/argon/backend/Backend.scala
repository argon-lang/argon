package dev.argon.backend

import dev.argon.compiler.*
import dev.argon.vm.resource.VmIrResourceContext

trait Backend {
  type Options[E]
  type Output[E]

  def name: String

  def codegen(
    context: BackendContext
  )(
    vmIrResContext: VmIrResourceContext & HasContext[context.type]
  )(
    options: Options[context.Error],
    program: vmIrResContext.VmIrResource[context.Error],
    libraries: Map[TubeName, vmIrResContext.VmIrResource[context.Error]],
  ): context.Comp[Output[context.Error]]

  def outputSink(context: BackendContext)(output: Output[context.Error]): context.Comp[Unit]
  def testExecutor: Option[TestExecutor[Output]]

}
