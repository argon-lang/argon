package dev.argon.compiler.backend

import dev.argon.io.*

trait ModuleLoader {
  val supportedExtensions: Seq[String]
  val resourceLoader: ResourceLoader[?]
}
