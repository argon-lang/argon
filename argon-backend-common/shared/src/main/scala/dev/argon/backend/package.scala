package dev.argon

import dev.argon.module._
import zio.Has

package object backend {
  type ResourceAccess = Has[ResourceAccess.Service]
}
