package dev.argon.options

import dev.argon.io.*
import java.io.IOException
import zio.*

trait OutputInfo[R, E, Options] {
  def getValue(options: Options): BinaryResource[R, E] | DirectoryResource[R, E, BinaryResource]
}

