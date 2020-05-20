package dev.argon

import dev.argon.compiler.loaders.{ResourceIndicator, ResourceReader}
import zio.Has

package object backend {
  type ResourceWriter[I <: ResourceIndicator] = Has[ResourceWriter.Service[I]]
  type ResourceAccess[I <: ResourceIndicator] = ResourceReader[I] with ResourceWriter[I]
}
