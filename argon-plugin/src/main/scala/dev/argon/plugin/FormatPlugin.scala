package dev.argon.plugin

import cats.data.OptionT
import dev.argon.util.{*, given}
import dev.argon.compiler.*
import dev.argon.options.{OptionDecoder, OutputHandler}
import zio.{ZEnvironment, ZIO}



abstract class FormatPlugin[E >: PluginError, Platforms <: PlatformPluginSet[E]] {
  val platforms: Platforms
  val pluginId: String


  type CompatibleContext = Context {
    type Env <: PluginEnv
    type Error = E
    val implementations: {
      type ExternFunctionImplementation = ZEnvironment[platforms.externFunction.Implementation]
      type FunctionReference = ZEnvironment[platforms.externFunction.Reference]
      type RecordReference = ZEnvironment[platforms.externRecord.Reference]
    }
  }

  def emitter[Ctx <: CompatibleContext]: Option[TubeEmitter[Ctx]]
  def tubeLoaders[Ctx <: CompatibleContext]: Map[String, TubeLoader[Ctx]]
}

private[plugin] sealed trait FormatPluginSet[E >: PluginError, Platforms <: PlatformPluginSet[E]] {
  val platforms: Platforms
  val pluginIds: Set[String]


  type CompatibleContext = Context {
    type Env <: PluginEnv
    type Error = E
    val implementations: {
      type ExternFunctionImplementation = ZEnvironment[platforms.externFunction.Implementation]
      type FunctionReference = ZEnvironment[platforms.externFunction.Reference]
      type RecordReference = ZEnvironment[platforms.externRecord.Reference]
    }
  }

  def emitter[Ctx <: CompatibleContext]: TubeEmitterSet[Ctx]
  def tubeLoaders[Ctx <: CompatibleContext]: Map[TubeLoaderName, TubeLoader[Ctx]]
}

private[plugin] object FormatPluginSet {
  final class Empty[E >: PluginError, Platforms <: PlatformPluginSet[E]](override val platforms: Platforms) extends FormatPluginSet[E, Platforms] {
    override val pluginIds: Set[String] = Set.empty

    override def emitter[Ctx <: CompatibleContext]: TubeEmitterSet[Ctx] =
      TubeEmitterSet.Empty()

    override def tubeLoaders[Ctx <: CompatibleContext]: Map[TubeLoaderName, TubeLoader[Ctx]] = Map.empty
  }

  final class Singleton[E >: PluginError, Platforms <: PlatformPluginSet[E]](val plugin: FormatPlugin[E, Platforms]) extends FormatPluginSet[E, Platforms] {
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

  private final class Union[E >: PluginError, Platforms <: PlatformPluginSet[E]](val aSet: FormatPluginSet[E, Platforms], val bSet: FormatPluginSet[E, aSet.platforms.type]) extends FormatPluginSet[E, Platforms] {
    override val platforms: aSet.platforms.type = aSet.platforms
    override val pluginIds: Set[String] = aSet.pluginIds | bSet.pluginIds

    override def emitter[Ctx <: CompatibleContext]: TubeEmitterSet[Ctx] =
      TubeEmitterSet.Union(aSet.emitter, bSet.emitter)

    override def tubeLoaders[Ctx <: CompatibleContext]: Map[TubeLoaderName, TubeLoader[Ctx]] =
      aSet.tubeLoaders ++ bSet.tubeLoaders
  }

  def union[E >: PluginError](platforms: PlatformPluginSet[E])(a: FormatPluginSet[E, platforms.type], b: FormatPluginSet[E, platforms.type]): FormatPluginSet[E, platforms.type] =
    (a, b) match {
      case (_: FormatPluginSet.Empty[E, platforms.type], _) => b
      case (_, _: FormatPluginSet.Empty[E, platforms.type]) => a
      case _ => Union(a, b)
    }
}
