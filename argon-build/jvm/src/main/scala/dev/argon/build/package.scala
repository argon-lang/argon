package dev.argon

import dev.argon.io.FileIO
import zio.blocking.Blocking

package object build {

  type BuildEnvironment = FileIO with Blocking

}
