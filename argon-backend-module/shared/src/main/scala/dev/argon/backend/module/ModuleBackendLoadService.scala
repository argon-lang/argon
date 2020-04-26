package dev.argon.backend.module

import dev.argon.backend.ResourceAccess
import dev.argon.compiler.core.PayloadSpecifiers.ReferencePayloadSpecifier
import dev.argon.compiler.loaders.ModuleLoad
import dev.argon.loaders.armodule.ArgonModuleLoader.PayloadLoader
import dev.argon.loaders.armodule.{AggregateLoadService, ArgonModuleLoader}
import zio.{Has, ZLayer}

final class ModuleBackendLoadService(res: ResourceAccess.Service) extends AggregateLoadService[ModuleContext] {

  implicit val payloadLoader: PayloadLoader[ModuleContext, ReferencePayloadSpecifier] = new ModulePayloadLoader

  override protected val loadServices: Vector[ModuleLoad.Service[ModuleContext]] =
    Vector(ArgonModuleLoader(res))
}

object ModuleBackendLoadService {

  val uponResourceAccess: ZLayer[ResourceAccess, Nothing, Has[ModuleBackendLoadService]] = ZLayer.fromFunction { env =>
    new ModuleBackendLoadService(env.get)
  }

}
