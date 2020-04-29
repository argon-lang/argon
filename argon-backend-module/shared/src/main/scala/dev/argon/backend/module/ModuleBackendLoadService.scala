package dev.argon.backend.module

import dev.argon.backend.ResourceReader
import dev.argon.compiler.core.Context
import dev.argon.compiler.core.PayloadSpecifiers.ReferencePayloadSpecifier
import dev.argon.compiler.loaders.{ModuleLoad, ResourceIndicator}
import dev.argon.loaders.armodule.ArgonModuleLoader.PayloadLoader
import dev.argon.loaders.armodule.{AggregateLoadService, ArgonModuleLoader}
import zio._

object ModuleBackendLoadService {

  def forResourceReader[I <: ResourceIndicator: Tagged, TContext <: ModuleContext with Context.WithRes[I]: Tagged]: ZLayer[ResourceReader[I], Nothing, ModuleLoad[I, TContext]] = ZLayer.fromFunction { env =>
    val res = env.get
    implicit val payloadLoader: PayloadLoader[TContext, ReferencePayloadSpecifier] = new ModulePayloadLoader[TContext]

    new AggregateLoadService[I, TContext](Vector(
      ArgonModuleLoader[I, TContext](res)(payloadLoader)
    ))
  }

}
