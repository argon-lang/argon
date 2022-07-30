package dev.argon.build

import dev.argon.compiler.*
import dev.argon.compiler.tube.{ArTubeC, TubeName}
import dev.argon.io.ResourceFactory
import dev.argon.options.OptionDecoder
import dev.argon.plugin.*
import dev.argon.util.toml.Toml
import zio.*
import zio.stm.{TMap, ZSTM}

private[build] sealed abstract class BuildContextFactory[R <: ResourceFactory & CompEnv, E >: BuildError | CompError] {
  type Options <: Tuple
  type ExternMethodImplementation <: Tuple

  given optionsHandler: OptionDecoder[E, Options]

  def getPlugin(name: String): Option[Plugin[R, E]] =
    getPluginHelper(name).map { _.plugin }


  trait PluginHelper {
    val plugin: Plugin[R, E]
    def getOptions(options: Options): plugin.Options[R, E]
    def getExternalMethodImplementation(impl: ExternMethodImplementation): plugin.ExternalMethodImplementation

    def adapter
    (ctx: Context {
      type Env = R
      type Error = E
      type Options = BuildContextFactory.this.Options
      type ExternMethodImplementation = BuildContextFactory.this.ExternMethodImplementation
    }): PluginContextAdapter.Aux[ctx.type, plugin.type] =
      new PluginContextAdapter {
        override val context: ctx.type = ctx
        override val plugin: PluginHelper.this.plugin.type = PluginHelper.this.plugin

        override def extractExternMethodImplementation(impl: ExternMethodImplementation): plugin.ExternalMethodImplementation =
          PluginHelper.this.getExternalMethodImplementation(impl)

      }
  }

  def getPluginHelper(name: String): Option[PluginHelper]


  def getExternMethodImplementation(options: Options, id: String): ZIO[R, Option[E], ExternMethodImplementation]


  final def createContext
  : UIO[Context with LoadTube {
    type Env = R
    type Error = E
    type Options = BuildContextFactory.this.Options
    type ExternMethodImplementation = BuildContextFactory.this.ExternMethodImplementation
  }] =
    ZIO.runtime[Any].flatMap { runtime =>
      ZIO.succeed {
        new Context with LoadTube {
          override type Env = R
          override type Error = E

          override type Options = BuildContextFactory.this.Options

          private val tubes =
            Unsafe.unsafe {
              runtime.unsafe.run(TMap.empty[TubeName, ArTubeC with HasContext[this.type]].commit).getOrThrow()
            }


          override type ExternMethodImplementation = BuildContextFactory.this.ExternMethodImplementation


          override def getTube(tubeName: TubeName): Comp[ArTubeC with HasContext[this.type]] =
            tubes.get(tubeName).commit
              .some
              .orElseFail(DiagnosticError.UnknownTube(tubeName))

          override def loadTube
          (tubeOptions: TubeOptions)
          : ZIO[R & Scope, E, ArTubeC with HasContext[this.type]] =
            for
              plugin <- ZIO.fromEither(getPlugin(tubeOptions.loader.plugin).toRight(UnknownPlugin(tubeOptions.loader.plugin)))
              loader <- ZIO.fromEither(plugin.tubeLoaders.get(tubeOptions.loader.name).toRight(UnknownTubeLoader(tubeOptions.loader)))

              libOptions <- loader.libOptionDecoder[E, Options]
                .decode(tubeOptions.options)
                .mapError(BuildConfigParseError.apply)

              tube <- loader.load(this)(libOptions)
              _ <- ZSTM.ifSTM(tubes.contains(tube.tubeName))(
                onTrue = ZSTM.fail(DuplicateTube(tube.tubeName)),
                onFalse = tubes.put(tube.tubeName, tube),
              ).commit
            yield tube

          override def getExternMethodImplementation(options: Options, id: String): ZIO[R, Option[E], ExternMethodImplementation] =
            BuildContextFactory.this.getExternMethodImplementation(options, id)
        }
      }
    }

}

private[build] object BuildContextFactory {
  def make[R <: ResourceFactory & CompEnv, E >: BuildError | CompError](plugins: Map[String, Plugin[R, E]], buildConfig: BuildConfig): ZIO[R, E, BuildContextFactory[R, E]] =
    def forPlugins(pluginNames: List[String]): ZIO[R, E, BuildContextFactory[R, E]] =
      pluginNames match {
        case Nil => ZIO.succeed(BuildContextFactoryNil())
        case pluginName :: remainingPlugins =>
          for
            plugin <- ZIO.fromEither(plugins.get(pluginName).toRight(UnknownPlugin(pluginName)))
            tailFactory <- forPlugins(remainingPlugins)
          yield BuildContextFactoryCons(pluginName, plugin, tailFactory)
      }

    forPlugins(buildConfig.plugins.toList)
  end make
}

private[build] final class BuildContextFactoryNil[R <: ResourceFactory & CompEnv, E >: BuildError | CompError] extends BuildContextFactory[R, E] {
  override type Options = EmptyTuple
  override type ExternMethodImplementation = EmptyTuple

  override given optionsHandler: OptionDecoder[E, Options] with
    override def decode(value: Toml): ZIO[ResourceFactory, String, Options] =
      ZIO.succeed(EmptyTuple)
  end optionsHandler


  override def getPluginHelper(name: String): Option[PluginHelper] = None

  override def getExternMethodImplementation(options: Options, id: String): ZIO[R, Option[E], ExternMethodImplementation] =
    ZIO.succeed(EmptyTuple)
}

private[build] final class BuildContextFactoryCons[R <: ResourceFactory & CompEnv, E >: BuildError | CompError]
(pluginName: String, val plugin: Plugin[R, E], val rest: BuildContextFactory[R, E]) extends BuildContextFactory[R, E] {
  override type Options = plugin.Options[R, E] *: rest.Options
  override type ExternMethodImplementation = plugin.ExternalMethodImplementation *: rest.ExternMethodImplementation

  override given optionsHandler: OptionDecoder[E, Options] with
    override def decode(value: Toml): ZIO[ResourceFactory, String, Options] =
      val pluginOptToml = value match {
        case Toml.Table(map) => map.get(pluginName).getOrElse(Toml.Table.empty)
        case _ => Toml.Table.empty
      }
      for
        pluginOpt <- plugin.optionDecoder[E].decode(pluginOptToml)
        tailOpt <- rest.optionsHandler.decode(value)
      yield pluginOpt *: tailOpt
  end optionsHandler


  override def getPluginHelper(name: String): Option[PluginHelper] =
    if name == pluginName then
      Some(new PluginHelper {
        override val plugin: BuildContextFactoryCons.this.plugin.type = BuildContextFactoryCons.this.plugin
        override def getOptions(options: Options): plugin.Options[R, E] =
          val (pluginOpts *: _) = options
          pluginOpts
        end getOptions

        override def getExternalMethodImplementation(impl: ExternMethodImplementation): plugin.ExternalMethodImplementation =
          val (headImpl *: _) = impl
          headImpl
        end getExternalMethodImplementation
      })
    else
      rest.getPluginHelper(name).map { restPluginHelper =>
        new PluginHelper {
          override val plugin: restPluginHelper.plugin.type = restPluginHelper.plugin
          override def getOptions(options: Options): plugin.Options[R, E] =
            val (_ *: restPluginOpts) = options
            restPluginHelper.getOptions(restPluginOpts)
          end getOptions

          override def getExternalMethodImplementation(impl: ExternMethodImplementation): plugin.ExternalMethodImplementation =
            val (_ *: tailImpl) = impl
            restPluginHelper.getExternalMethodImplementation(tailImpl)
          end getExternalMethodImplementation
        }
      }

  override def getPlugin(name: String): Option[Plugin[R, E]] =
    if name == pluginName then Some(plugin)
    else rest.getPlugin(name)

  override def getExternMethodImplementation(options: Options, id: String): ZIO[R, Option[E], ExternMethodImplementation] =
    val (configHead *: configTail) = options
    for
      head <- plugin.loadExternMethod(configHead)(id).some
      tail <- rest.getExternMethodImplementation(configTail, id)
    yield head *: tail
  end getExternMethodImplementation

}

