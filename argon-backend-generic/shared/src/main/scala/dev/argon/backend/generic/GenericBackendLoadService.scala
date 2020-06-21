package dev.argon.backend.generic

import dev.argon.compiler.core.Context
import dev.argon.compiler.core.PayloadSpecifiers.ReferencePayloadSpecifier
import dev.argon.compiler.loaders.{ModuleLoad, ResourceIndicator, ResourceReader}
import dev.argon.armodule.loader.ArgonModuleLoader.PayloadLoader
import dev.argon.armodule.loader.{AggregateLoadService, ArgonModuleLoader}
import zio._

private[generic] object GenericBackendLoadService {

  def forResourceReader[I <: ResourceIndicator: Tag, TContext <: ModuleContext with Context.WithRes[I]: Tag]: ZLayer[ResourceReader[I], Nothing, ModuleLoad[I, TContext]] = ZLayer.fromFunction { env =>
    val res = env.get
    implicit val payloadLoader: PayloadLoader[TContext, ReferencePayloadSpecifier] = new GenericPayloadLoader[TContext]

    new AggregateLoadService[I, TContext](Vector(
      ArgonModuleLoader[I, TContext](res)(payloadLoader)
    ))
  }

}
