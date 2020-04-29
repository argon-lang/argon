package dev.argon

import dev.argon.compiler.loaders.ResourceIndicator
import dev.argon.module._
import zio.Has

package object backend {
  type ResourceReader[I <: ResourceIndicator] = Has[ResourceReader.Service[I]]
  type ResourceWriter[I <: ResourceIndicator] = Has[ResourceWriter.Service[I]]
  type ResourceAccess[I <: ResourceIndicator] = ResourceReader[I] with ResourceWriter[I]
}
