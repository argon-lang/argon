package dev.argon.backend.js

import dev.argon.backend.ResourceAccess
import dev.argon.compiler.core.PayloadSpecifiers.ReferencePayloadSpecifier
import dev.argon.compiler.loaders.ModuleLoad
import dev.argon.loaders.armodule.{AggregateLoadService, ArgonModuleLoader}
import dev.argon.loaders.armodule.ArgonModuleLoader.PayloadLoader
import zio.{Has, ZLayer}

final class JSBackendLoadService(res: ResourceAccess.Service) extends AggregateLoadService[JSContext] {


  implicit val payloadLoader: PayloadLoader[JSContext, ReferencePayloadSpecifier] = new JSPayloadLoader

  override protected val loadServices: Vector[ModuleLoad.Service[JSContext]] =
    Vector(ArgonModuleLoader(res))
}

object JSBackendLoadService {

  val uponResourceAccess: ZLayer[ResourceAccess, Nothing, Has[JSBackendLoadService]] = ZLayer.fromFunction { env =>
    new JSBackendLoadService(env.get)
  }

}
