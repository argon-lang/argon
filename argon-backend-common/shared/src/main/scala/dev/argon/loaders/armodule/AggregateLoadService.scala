package dev.argon.loaders.armodule

import dev.argon.compiler.ErrorList
import dev.argon.compiler.core.Context
import dev.argon.compiler.loaders.{ModuleLoad, ModuleMetadata, ResourceIndicator}
import zio.Managed
import cats.implicits._
import zio.interop.catz._

abstract class AggregateLoadService[TContext <: Context] extends ModuleLoad.Service[TContext] {

  override def loadResource(id: ResourceIndicator): Managed[ErrorList, Option[ModuleMetadata[TContext]]] =
    loadServices.collectFirstSomeM { _.loadResource(id) }

  protected val loadServices: Vector[ModuleLoad.Service[TContext]]
}
