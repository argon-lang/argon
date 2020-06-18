package dev.argon.armodule.loader

import dev.argon.compiler.ErrorList
import dev.argon.compiler.core.Context
import dev.argon.compiler.loaders.{ModuleLoad, ModuleMetadata, ResourceIndicator}
import zio.Managed
import cats.implicits._
import zio.interop.catz.core._

final class AggregateLoadService[I <: ResourceIndicator, TContext <: Context.WithRes[I]](loadServices: Vector[ModuleLoad.Service[I, TContext]]) extends ModuleLoad.Service[I, TContext] {

  override def loadResource(id: I): Managed[ErrorList, Option[ModuleMetadata[TContext]]] =
    loadServices.collectFirstSomeM { _.loadResource(id) }

}
