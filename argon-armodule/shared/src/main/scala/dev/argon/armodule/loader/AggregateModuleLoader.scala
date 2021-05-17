package dev.argon.armodule.loader

import dev.argon.compiler.CompManaged
import dev.argon.compiler.loaders.ModuleLoader
import cats.implicits._
import dev.argon.compiler.output.ArgonModuleSerialized
import zio.interop.catz.core._

final class AggregateModuleLoader(loadServices: Vector[ModuleLoader]) extends ModuleLoader {
  override val supportedExtensions: Seq[String] = loadServices.flatMap(_.supportedExtensions)

  override def loadResource(fileName: String): CompManaged[Option[ArgonModuleSerialized]] =
    loadServices.collectFirstSomeM { _.loadResource(fileName) }
}
