package dev.argon.backend.js

import dev.argon.backend.ResourceReader
import dev.argon.compiler.core.Context
import dev.argon.compiler.core.PayloadSpecifiers.ReferencePayloadSpecifier
import dev.argon.compiler.loaders.{ModuleLoad, ResourceIndicator}
import dev.argon.loaders.armodule.{AggregateLoadService, ArgonModuleLoader}
import dev.argon.loaders.armodule.ArgonModuleLoader.PayloadLoader
import zio._

object JSBackendLoadService {

  def forResourceReader[I <: ResourceIndicator: Tagged, TContext <: JSContext with Context.WithRes[I]: Tagged]: ZLayer[ResourceReader[I], Nothing, ModuleLoad[I, TContext]] = ZLayer.fromFunction { env =>
    val res = env.get
    implicit val payloadLoader: PayloadLoader[TContext, ReferencePayloadSpecifier] = new JSPayloadLoader[TContext]

    new AggregateLoadService[I, TContext](Vector(
      ArgonModuleLoader[I, TContext](res)(payloadLoader)
    ))
  }

}
