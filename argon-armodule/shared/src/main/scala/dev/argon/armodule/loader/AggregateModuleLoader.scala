package dev.argon.armodule.loader

import dev.argon.compiler.{CompManaged, CompilationError}
import dev.argon.compiler.core.Context
import dev.argon.compiler.loaders.{ModuleLoader, UnlinkedModule}
import zio.Managed
import cats.implicits._
import dev.argon.compiler.core.PayloadSpecifiers.ReferencePayloadSpecifier
import zio.interop.catz.core._

final class AggregateModuleLoader[TContext <: Context](loadServices: Vector[ModuleLoader[TContext]]) extends ModuleLoader[TContext] {


  override def loadResource(fileName: String): CompManaged[Option[UnlinkedModule[TContext, ReferencePayloadSpecifier]]] =
    loadServices.collectFirstSomeM { _.loadResource(fileName) }

}
