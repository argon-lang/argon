package dev.argon.options

import dev.argon.io.*
import java.io.IOException
import zio.*

trait OutputInfo[+E, -Options] {
  def getValue(options: Options): FileSystemResource[E, BinaryResource]
}

