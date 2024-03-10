package dev.argon.plugin

import cats.data.OptionT
import dev.argon.util.{*, given}
import dev.argon.compiler.*
import dev.argon.options.{OptionDecoder, OutputHandler}
import zio.{ZEnvironment, ZIO}



abstract class FormatPlugin[Platforms <: PlatformPluginSet] {
  val platforms: Platforms
  val pluginId: String


  type CompatibleContext = Context {
    type Env <: PluginEnv
    type Error >: PluginError
    val implementations: {
      type ExternFunctionImplementation = ZEnvironment[platforms.externFunction.Implementation]
      type FunctionReference = ZEnvironment[platforms.externFunction.Reference]
    }
  }

  def emitter[Ctx <: CompatibleContext]: Option[TubeEmitter[Ctx]]
  def tubeLoaders[Ctx <: CompatibleContext]: Map[String, TubeLoader[Ctx]]
}

private[plugin] sealed trait FormatPluginSet[Platforms <: PlatformPluginSet] {
  val platforms: Platforms
  val pluginIds: Set[String]


  type CompatibleContext = Context {
    type Env <: PluginEnv
    type Error >: PluginError
    val implementations: {
      type ExternFunctionImplementation = ZEnvironment[platforms.externFunction.Implementation]
      type FunctionReference = ZEnvironment[platforms.externFunction.Reference]
    }
  }

  def emitter[Ctx <: CompatibleContext]: TubeEmitterSet[Ctx]
  def tubeLoaders[Ctx <: CompatibleContext]: Map[TubeLoaderName, TubeLoader[Ctx]]
}

private[plugin] object FormatPluginSet {
  final class Empty[Platforms <: PlatformPluginSet](override val platforms: Platforms) extends FormatPluginSet[Platforms] {
    override val pluginIds: Set[String] = Set.empty

    override def emitter[Ctx <: CompatibleContext]: TubeEmitterSet[Ctx] =
      TubeEmitterSet.Empty()

    override def tubeLoaders[Ctx <: CompatibleContext]: Map[TubeLoaderName, TubeLoader[Ctx]] = Map.empty
  }

  final class Singleton[Platforms <: PlatformPluginSet](val plugin: FormatPlugin[Platforms]) extends FormatPluginSet[Platforms] {
    override val platforms: plugin.platforms.type = plugin.platforms
    override val pluginIds: Set[String] = Set(plugin.pluginId)

    override def emitter[Ctx <: CompatibleContext]: TubeEmitterSet[Ctx] =
      plugin.emitter[Ctx] match {
        case Some(emitter) => TubeEmitterSet.Singleton(emitter, plugin.pluginId)
        case None => TubeEmitterSet.Empty()
      }

    override def tubeLoaders[Ctx <: CompatibleContext]: Map[TubeLoaderName, TubeLoader[Ctx]] =
      plugin.tubeLoaders[Ctx].map { (k, v) => TubeLoaderName(plugin.pluginId, k) -> v }
  }

  private final class Union[Platforms <: PlatformPluginSet](val aSet: FormatPluginSet[Platforms], val bSet: FormatPluginSet[aSet.platforms.type]) extends FormatPluginSet[Platforms] {
    override val platforms: aSet.platforms.type = aSet.platforms
    override val pluginIds: Set[String] = aSet.pluginIds | bSet.pluginIds

    override def emitter[Ctx <: CompatibleContext]: TubeEmitterSet[Ctx] =
      TubeEmitterSet.Union(aSet.emitter, bSet.emitter)

    override def tubeLoaders[Ctx <: CompatibleContext]: Map[TubeLoaderName, TubeLoader[Ctx]] =
      aSet.tubeLoaders ++ bSet.tubeLoaders
  }

  def union(platforms: PlatformPluginSet)(a: FormatPluginSet[platforms.type], b: FormatPluginSet[platforms.type]): FormatPluginSet[platforms.type] =
    (a, b) match {
      case (_: FormatPluginSet.Empty[platforms.type], _) => b
      case (_, _: FormatPluginSet.Empty[platforms.type]) => a
      case _ => Union(a, b)
    }
}
