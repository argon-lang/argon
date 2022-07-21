package dev.argon.plugin

import dev.argon.util.*
import dev.argon.io.*
import zio.*
import java.io.IOException

trait TubeLoader[Options[_, _]] {
  val supportedExtensions: Seq[String]
  
  def load[R, E](options: Options[R, E])(resource: BinaryResource[R, E]): ZIO[R & Scope, E, SerializedTube[R, E]]
}
