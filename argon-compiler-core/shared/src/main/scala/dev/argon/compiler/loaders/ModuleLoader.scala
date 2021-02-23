package dev.argon.compiler.loaders

import dev.argon.compiler._
import dev.argon.compiler.core.PayloadSpecifiers.ReferencePayloadSpecifier
import dev.argon.compiler.core._
import dev.argon.compiler.output.ArgonModuleSerialized
import zio._

trait ModuleLoader {
  val supportedExtensions: Seq[String]
  def loadResource(fileName: String): CompManaged[Option[ArgonModuleSerialized]]
}
