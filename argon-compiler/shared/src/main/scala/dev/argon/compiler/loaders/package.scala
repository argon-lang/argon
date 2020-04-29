package dev.argon.compiler

import dev.argon.compiler.core.Context
import zio.Has

package object loaders {
  type ModuleLoad[I <: ResourceIndicator, TContext <: Context] = Has[ModuleLoad.Service[I, TContext]]
}
