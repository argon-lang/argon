package dev.argon.backend.js

import dev.argon.compiler.core.Context
import dev.argon.compiler.core.PayloadSpecifiers.ReferencePayloadSpecifier
import dev.argon.compiler.loaders.{ModuleLoad, ResourceIndicator, ResourceReader}
import dev.argon.loaders.armodule.{AggregateLoadService, ArgonModuleLoader}
import dev.argon.loaders.armodule.ArgonModuleLoader.PayloadLoader
import zio._

object JSBackendLoadService {

  def forResourceReader[I <: ResourceIndicator: Tag, TContext <: JSContext with Context.WithRes[I]: Tag]: ZLayer[ResourceReader[I], Nothing, ModuleLoad[I, TContext]] = ZLayer.fromFunction { env =>
    val res = env.get
    implicit val payloadLoader: PayloadLoader[TContext, ReferencePayloadSpecifier] = new JSPayloadLoader[TContext]

    new AggregateLoadService[I, TContext](Vector(
      ArgonModuleLoader[I, TContext](res)(payloadLoader)
    ))
  }

}
