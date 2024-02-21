package dev.argon.plugin

import dev.argon.argonvm.VMWithTube
import dev.argon.compiler.{Context, HasContext}
import dev.argon.compiler.definitions.HasImplementation
import dev.argon.compiler.tube.ArTubeC

trait EmittableTube[R, E, TContext <: Context { type Env = R; type Error = E }] {
  def asTypedTube: ArTubeC & HasContext[TContext] & HasImplementation[true]
  def asVMTube: VMWithTube[R, E]
}
