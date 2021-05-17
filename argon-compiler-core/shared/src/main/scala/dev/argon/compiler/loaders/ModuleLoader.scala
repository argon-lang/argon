package dev.argon.compiler.loaders

import dev.argon.compiler._
import dev.argon.compiler.output.ArgonModuleSerialized

trait ModuleLoader {
  val supportedExtensions: Seq[String]
  def loadResource(fileName: String): CompManaged[Option[ArgonModuleSerialized]]
}
