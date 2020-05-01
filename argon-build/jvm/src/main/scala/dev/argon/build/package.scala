package dev.argon

import dev.argon.io.fileio.{FileIO, FileIOLite}
import zio.blocking.Blocking

package object build {

  type BuildEnvironment = FileIO with FileIOLite with Blocking

}
