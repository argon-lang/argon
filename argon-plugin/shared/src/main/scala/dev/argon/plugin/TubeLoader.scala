package dev.argon.plugin

import dev.argon.util.*
import dev.argon.io.*
import dev.argon.options.OptionHandler
import zio.*
import java.io.IOException

trait TubeLoader[Options[_[_[_]]]] {
  val supportedExtensions: Seq[String]
  
  def load[E](options: OptionHandler.WithRes[Options, E])(resource: BinaryResource[E]): ZIO[Scope, E, SerializedTube[E]]
}
