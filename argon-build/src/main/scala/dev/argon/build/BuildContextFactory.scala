package dev.argon.build

import dev.argon.compiler.*
import dev.argon.compiler.tube.{ArTubeC, TubeImporter, TubeName}
import dev.argon.io.{ResourceFactory, ResourceRecorder}
import dev.argon.options.OptionCodec
import dev.argon.plugin.*
import dev.argon.util.toml.Toml
import zio.*
import zio.stm.{TMap, ZSTM}

private[build] sealed abstract class BuildContextFactory[R <: ResourceFactory & CompEnv, E >: BuildError | CompError] {
  type Options <: Tuple
  type ExternMethodImplementation <: Tuple
  type ExternFunctionImplementation <: Tuple
  type ExternClassConstructorImplementation <: Tuple

  given optionsHandler: OptionCodecTable[R, E, Options]

  def getPlugin(name: String): Option[Plugin[R, E]] =
    getPluginHelper(name).map { _.plugin }

  type ContextRefined = Context {
    type Env = R
    type Error = E
    type Options = BuildContextFactory.this.Options
    type ExternMethodImplementation = BuildContextFactory.this.ExternMethodImplementation
    type ExternFunctionImplementation = BuildContextFactory.this.ExternFunctionImplementation
    type ExternClassConstructorImplementation = BuildContextFactory.this.ExternClassConstructorImplementation
  }

  trait PluginHelper {
    val plugin: Plugin[R, E]
    def getOptions(options: Options): plugin.Options[R, E]
    def getExternMethodImplementation(impl: ExternMethodImplementation): plugin.ExternMethodImplementation
    def getExternFunctionImplementation(impl: ExternFunctionImplementation): plugin.ExternFunctionImplementation
    def getExternClassConstructorImplementation(impl: ExternClassConstructorImplementation): plugin.ExternClassConstructorImplementation

    def adapter(ctx: ContextRefined): PluginContextAdapter.Aux[ctx.type, plugin.type] =
      new PluginContextAdapter {
        override val context: ctx.type = ctx
        override val plugin: PluginHelper.this.plugin.type = PluginHelper.this.plugin

        override def extractExternMethodImplementation(impl: ExternMethodImplementation): plugin.ExternMethodImplementation =
          PluginHelper.this.getExternMethodImplementation(impl)

        override def extractExternFunctionImplementation(impl: ExternFunctionImplementation): plugin.ExternFunctionImplementation =
          PluginHelper.this.getExternFunctionImplementation(impl)

        override def extractExternClassConstructorImplementation(impl: ExternClassConstructorImplementation): plugin.ExternClassConstructorImplementation =
          PluginHelper.this.getExternClassConstructorImplementation(impl)
      }
  }

  def getPluginHelper(name: String): Option[PluginHelper]


  def getExternMethodImplementation(options: Options, id: String): ZIO[R, Option[E], ExternMethodImplementation]
  def getExternFunctionImplementation(options: Options, id: String): ZIO[R, Option[E], ExternFunctionImplementation]
  def getExternClassConstructorImplementation(options: Options, id: String): ZIO[R, Option[E], ExternClassConstructorImplementation]


  final def createContext: ContextRefined =
    new Context {
      override type Env = R
      override type Error = E

      override type Options = BuildContextFactory.this.Options

      override type ExternMethodImplementation = BuildContextFactory.this.ExternMethodImplementation
      override type ExternFunctionImplementation = BuildContextFactory.this.ExternFunctionImplementation
      override type ExternClassConstructorImplementation = BuildContextFactory.this.ExternClassConstructorImplementation

      override def getExternMethodImplementation(options: Options, id: String): ZIO[R, Option[E], ExternMethodImplementation] =
        BuildContextFactory.this.getExternMethodImplementation(options, id)

      override def getExternFunctionImplementation(options: Options, id: String): ZIO[R, Option[E], ExternFunctionImplementation] =
        BuildContextFactory.this.getExternFunctionImplementation(options, id)

      override def getExternClassConstructorImplementation(options: Options, id: String): ZIO[R, Option[E], ExternClassConstructorImplementation] =
        BuildContextFactory.this.getExternClassConstructorImplementation(options, id)
    }

  final def createTubeImporter(ctx: ContextRefined): UIO[TubeImporter & LoadTube & HasContext[ctx.type]] =
    for
      tubes <- TMap.empty[TubeName, ArTubeC & HasContext[ctx.type]].commit
    yield new TubeImporter with LoadTube {
      override val context: ctx.type = ctx
      override def getTube(tubeName: TubeName): Comp[ArTube] =
        tubes.get(tubeName).commit
          .some
          .orElseFail(DiagnosticError.UnknownTube(tubeName))

      override def loadTube(tubeOptions: TubeOptions): ZIO[R & Scope, E, ArTube] =
        for
          plugin <- ZIO.fromEither(getPlugin(tubeOptions.loader.plugin).toRight(UnknownPlugin(tubeOptions.loader.plugin)))
          loader <- ZIO.fromEither(plugin.tubeLoaders.get(tubeOptions.loader.name).toRight(UnknownTubeLoader(tubeOptions.loader)))

          libOptions <- loader.libOptionDecoder[R, E, Options]
            .decode(tubeOptions.options)
            .mapError(BuildConfigParseError.apply)

          tube <- loader.load(ctx)(this)(libOptions)
          _ <- ZSTM.ifSTM(tubes.contains(tube.tubeName))(
            onTrue = ZSTM.fail(DuplicateTube(tube.tubeName)),
            onFalse = tubes.put(tube.tubeName, tube),
          ).commit
        yield tube
    }


}

private[build] trait OptionCodecTable[R, E, A] extends OptionCodec[R, E, A] {
  override def encode(value: A): ZIO[ResourceRecorder & R, E, Toml.Table]
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
  override type ExternFunctionImplementation = EmptyTuple
  override type ExternClassConstructorImplementation = EmptyTuple

  override given optionsHandler: OptionCodecTable[R, E, Options] with
    override def decode(value: Toml): ZIO[ResourceFactory, String, Options] =
      ZIO.succeed(EmptyTuple)

    override def encode(value: Options): ZIO[ResourceRecorder & R, E, Toml.Table] =
      ZIO.succeed(Toml.Table.empty)
  end optionsHandler


  override def getPluginHelper(name: String): Option[PluginHelper] = None

  override def getExternMethodImplementation(options: Options, id: String): ZIO[R, Option[E], ExternMethodImplementation] =
    ZIO.succeed(EmptyTuple)

  override def getExternFunctionImplementation(options: Options, id: String): ZIO[R, Option[E], ExternFunctionImplementation] =
    ZIO.succeed(EmptyTuple)

  override def getExternClassConstructorImplementation(options: Options, id: String): ZIO[R, Option[E], ExternClassConstructorImplementation] =
    ZIO.succeed(EmptyTuple)

}

private[build] final class BuildContextFactoryCons[R <: ResourceFactory & CompEnv, E >: BuildError | CompError]
(pluginName: String, val plugin: Plugin[R, E], val rest: BuildContextFactory[R, E]) extends BuildContextFactory[R, E] {
  override type Options = plugin.Options[R, E] *: rest.Options
  override type ExternMethodImplementation = plugin.ExternMethodImplementation *: rest.ExternMethodImplementation
  override type ExternFunctionImplementation = plugin.ExternFunctionImplementation *: rest.ExternFunctionImplementation
  override type ExternClassConstructorImplementation = plugin.ExternClassConstructorImplementation *: rest.ExternClassConstructorImplementation

  override given optionsHandler: OptionCodecTable[R, E, Options] with
    override def decode(value: Toml): ZIO[ResourceFactory, String, Options] =
      val pluginOptToml = value match {
        case Toml.Table(map) => map.get(pluginName).getOrElse(Toml.Table.empty)
        case _ => Toml.Table.empty
      }
      for
        pluginOpt <- plugin.optionCodec[R, E].decode(pluginOptToml)
        tailOpt <- rest.optionsHandler.decode(value)
      yield pluginOpt *: tailOpt
    end decode

    override def encode(value: plugin.Options[R, E] *: rest.Options): ZIO[ResourceRecorder & R, E, Toml.Table] =
      val (head *: tail) = value
      for
        headToml <- plugin.optionCodec[R, E].encode(head)
        tailToml <- rest.optionsHandler.encode(tail)
      yield tailToml
    end encode
  end optionsHandler


  override def getPluginHelper(name: String): Option[PluginHelper] =
    if name == pluginName then
      Some(new PluginHelper {
        override val plugin: BuildContextFactoryCons.this.plugin.type = BuildContextFactoryCons.this.plugin
        override def getOptions(options: Options): plugin.Options[R, E] =
          val (pluginOpts *: _) = options
          pluginOpts
        end getOptions

        override def getExternMethodImplementation(impl: ExternMethodImplementation): plugin.ExternMethodImplementation =
          val (headImpl *: _) = impl
          headImpl
        end getExternMethodImplementation

        override def getExternFunctionImplementation(impl: ExternFunctionImplementation): plugin.ExternFunctionImplementation =
          val (headImpl *: _) = impl
          headImpl
        end getExternFunctionImplementation

        override def getExternClassConstructorImplementation(impl: ExternClassConstructorImplementation): plugin.ExternClassConstructorImplementation =
          val (headImpl *: _) = impl
          headImpl
        end getExternClassConstructorImplementation
      })
    else
      rest.getPluginHelper(name).map { restPluginHelper =>
        new PluginHelper {
          override val plugin: restPluginHelper.plugin.type = restPluginHelper.plugin
          override def getOptions(options: Options): plugin.Options[R, E] =
            val (_ *: restPluginOpts) = options
            restPluginHelper.getOptions(restPluginOpts)
          end getOptions

          override def getExternMethodImplementation(impl: ExternMethodImplementation): plugin.ExternMethodImplementation =
            val (_ *: tailImpl) = impl
            restPluginHelper.getExternMethodImplementation(tailImpl)
          end getExternMethodImplementation

          override def getExternFunctionImplementation(impl: ExternFunctionImplementation): plugin.ExternFunctionImplementation =
            val (_ *: tailImpl) = impl
            restPluginHelper.getExternFunctionImplementation(tailImpl)
          end getExternFunctionImplementation

          override def getExternClassConstructorImplementation(impl: ExternClassConstructorImplementation): plugin.ExternClassConstructorImplementation =
            val (_ *: tailImpl) = impl
            restPluginHelper.getExternClassConstructorImplementation(tailImpl)
          end getExternClassConstructorImplementation
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

  override def getExternFunctionImplementation(options: Options, id: String): ZIO[R, Option[E], ExternFunctionImplementation] =
    val (configHead *: configTail) = options
    for
      head <- plugin.loadExternFunction(configHead)(id).some
      tail <- rest.getExternFunctionImplementation(configTail, id)
    yield head *: tail
  end getExternFunctionImplementation

  override def getExternClassConstructorImplementation(options: Options, id: String): ZIO[R, Option[E], ExternClassConstructorImplementation] =
    val (configHead *: configTail) = options
    for
      head <- plugin.loadExternClassConstructor(configHead)(id).some
      tail <- rest.getExternClassConstructorImplementation(configTail, id)
    yield head *: tail
  end getExternClassConstructorImplementation

}

