package dev.argon.build

import dev.argon.plugin.*
import dev.argon.plugin.platform.*
import zio.*

final class PluginLoader[R <: AnyPluginEnv, E >: AnyPluginError | BuildError](val platformPlugins: PlatformPluginSet[R, E]) {
  import PluginLoader.getPluginFactory

  type PluginForPlatforms = Plugin[R, E] {
    type Options = platformPlugins.Options
    val externMethod: platformPlugins.externMethod.type
    val externFunction: platformPlugins.externFunction.type
    val externClassConstructor: platformPlugins.externClassConstructor.type
  }

  def loadInputPlugin(id: String): ZIO[R, E, PluginForPlatforms] =
    platformPlugins match {
      case plugin: (PlatformPlugin[R, E] & platformPlugins.type) if plugin.pluginId == id =>
        ZIO.succeed(plugin : PluginForPlatforms)

      case _ =>
        getPluginFactory[R, E](id)
          .flatMap {
            case factory: CompositePluginFactory[R, E] =>
              factory.make(platformPlugins)
              
            case _: PlatformPluginFactory[R, E] =>
              ZIO.fail(InvalidPluginForPlatforms(id, platformPlugins.platformIds))
          }
    }

  def loadOutputPlugin[TPlugin <: PluginForPlatforms](id: String)(context: PluginContext[R, E, TPlugin]): ZIO[R, E, PluginWithAdapter[R, E, context.plugin.type]] =
    getPluginFactory[R, E](id)
      .flatMap {
        case factory: CompositePluginFactory[R, E] =>
          factory.make(platformPlugins).map { newPlugin =>
            new PluginWithAdapter[R, E, context.plugin.type] {
              override val plugin: newPlugin.type = newPlugin
              override val adapter: PluginAdapter[R, E, context.plugin.type, newPlugin.type] =
                new PluginAdapter[R, E, context.plugin.type, newPlugin.type] {
                  override val ctxPlugin: context.plugin.type = context.plugin
                  override val plugin: newPlugin.type = newPlugin

                  override def getExternMethodImplementation(method: ctxPlugin.externMethod.Implementation): plugin.externMethod.Implementation =
                    method

                  override def getExternMethodReference(method: ctxPlugin.externMethod.Reference): plugin.externMethod.Reference =
                    method

                  override def getExternFunctionImplementation(func: ctxPlugin.externFunction.Implementation): plugin.externFunction.Implementation =
                    func

                  override def getExternFunctionReference(func: ctxPlugin.externFunction.Reference): plugin.externFunction.Reference =
                    func

                  override def getExternClassConstructorImplementation(ctor: ctxPlugin.externClassConstructor.Implementation): plugin.externClassConstructor.Implementation =
                    ctor

                  override def getExternClassConstructorReference(ctor: ctxPlugin.externClassConstructor.Reference): plugin.externClassConstructor.Reference =
                    ctor
                }
            }
          }

        case _: PlatformPluginFactory[R, E] =>
          (platformPlugins : PlatformPluginSet[R, E] & platformPlugins.type) match {
            case platformPlugin: (PlatformPlugin[R, E] & platformPlugins.type) =>
              ZIO.succeed(new PluginWithAdapter[R, E, context.plugin.type] {
                override val plugin: platformPlugin.type = platformPlugin
                override val adapter: PluginAdapter[R, E, context.plugin.type, platformPlugin.type] =
                  new PluginAdapter[R, E, context.plugin.type, platformPlugin.type] {
                    override val ctxPlugin: context.plugin.type = context.plugin
                    override val plugin: platformPlugin.type = platformPlugin

                    override def getExternMethodImplementation(method: ctxPlugin.externMethod.Implementation): plugin.externMethod.Implementation =
                      method

                    override def getExternMethodReference(method: ctxPlugin.externMethod.Reference): plugin.externMethod.Reference =
                      method

                    override def getExternFunctionImplementation(func: ctxPlugin.externFunction.Implementation): plugin.externFunction.Implementation =
                      func

                    override def getExternFunctionReference(func: ctxPlugin.externFunction.Reference): plugin.externFunction.Reference =
                      func

                    override def getExternClassConstructorImplementation(ctor: ctxPlugin.externClassConstructor.Implementation): plugin.externClassConstructor.Implementation =
                      ctor

                    override def getExternClassConstructorReference(ctor: ctxPlugin.externClassConstructor.Reference): plugin.externClassConstructor.Reference =
                      ctor
                  }
              })

            case set: (PluginSet[R, E] & platformPlugins.type) =>
              ZIO.fromEither(
                set.getWithAdapter(id, context.plugin)
                  .toRight { InvalidPluginForPlatforms(id, platformPlugins.platformIds) }
              )
          }

      }

}

object PluginLoader {
  def make[R <: AnyPluginEnv, E >: AnyPluginError | BuildError](buildConfig: BuildConfig): ZIO[R, E, PluginLoader[R, E]] =
    getPluginFactory[R, E](buildConfig.tube.loader.plugin)
      .flatMap {
        case factory: CompositePluginFactory[R, E] =>
          def loadPlatformPlugins: ZIO[R, E, PlatformPluginSet[R, E]] =
            buildConfig.platforms match {
              case Seq(platform) =>
                loadPlatformPlugin(platform)

              case _ =>
                for
                  partialPlugins <-
                    ZIO.foldRight[R, E, PluginSet.PartialPlugin[R, E], String](buildConfig.platforms)(PluginSet.PartialPluginNil[R, E]()) { (platform, partialPlugins) =>
                      for
                        plugin <- loadPlatformPlugin[R, E](platform)
                      yield PluginSet.PartialPluginCons[R, E](plugin, partialPlugins)
                    }
                yield PluginSet(partialPlugins)
            }

          for
            platformPlugins <- loadPlatformPlugins
          yield PluginLoader[R, E](platformPlugins)

        case factory: PlatformPluginFactory[R, E] if buildConfig.platforms == Seq(factory.pluginId) =>
          for
            p <- factory.make
          yield PluginLoader[R, E](p)

        case factory: PlatformPluginFactory[R, E] =>
          ZIO.fail(InvalidPluginForPlatforms(factory.pluginId, buildConfig.platforms))
      }

  private def getPluginFactory[R <: AnyPluginEnv, E >: AnyPluginError | BuildError](id: String): IO[BuildError, PluginFactory[R, E]] =
    ZIO.fromEither(pluginFactories[R, E].get(id).toRight(UnknownPlugin(id)))

  def loadPlatformPlugin[R <: AnyPluginEnv, E >: AnyPluginError | BuildError](id: String): ZIO[R, E, PlatformPlugin[R, E]] =
    getPluginFactory[R, E](id).flatMap {
      case factory: PlatformPluginFactory[R, E] =>
        factory.make

      case _ =>
        ZIO.fail(ExpectedPlatformPlugin(id))
    }



}


