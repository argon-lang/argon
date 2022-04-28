package dev.argon.plugin

import dev.argon.io.*
import zio.*
import java.io.IOException

trait TubeLoader[E, Options] {
  val supportedExtensions: Seq[String]
  
  def load(options: Options)(resource: BinaryResource[E]): ZIO[Scope, E, SerializedTube[E]]
}
