package dev.argon.options

import dev.argon.io.*
import java.io.IOException
import zio.*

trait OutputInfo[R, E, Options] {
  val name: Seq[String]
  def getValue(options: Options): BinaryResource[R, E] | DirectoryResource[R, E, BinaryResource]
}

