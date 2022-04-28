package dev.argon.options

import dev.argon.io.BinaryResource
import java.io.IOException
import zio.*

trait OutputInfo[A, Options] {
  val name: String
  val description: String

  def getValue(options: Options): A
}

