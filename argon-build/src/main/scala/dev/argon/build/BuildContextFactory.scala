package dev.argon.build

import dev.argon.compiler.*
import dev.argon.compiler.tube.{ArTubeC, TubeImporter, TubeName}
import dev.argon.esexpr.{Dictionary, ESExpr, ESExprCodec, ESExprTag}
import dev.argon.io.{ResourceFactory, ResourceRecorder}
import dev.argon.options.OptionCodec
import dev.argon.plugin.*
import dev.argon.util.toml.Toml
import zio.*
import zio.stm.{TMap, ZSTM}

private[build] sealed abstract class BuildContextFactory[R <: CompEnv, E >: BuildError | CompError] {
  type Options <: Tuple
  type ExternMethodImplementation <: Tuple
  type ExternFunctionImplementation <: Tuple
  type ExternClassConstructorImplementation <: Tuple

  given optionsHandler: OptionMapCodec[R, E, Options]

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
    def getOptions(options: Options): plugin.Options
    def getExternMethodImplementation(impl: ExternMethodImplementation): plugin.ExternMethodImplementation
    def getExternFunctionImplementation(impl: ExternFunctionImplementation): plugin.ExternFunctionImplementation
    def getExternClassConstructorImplementation(impl: ExternClassConstructorImplementation): plugin.ExternClassConstructorImplementation

    def adapter(ctx: ContextRefined): PluginContextAdapter.Aux[ctx.type, plugin.type] =
      new PluginContextAdapter {
        override val context: ctx.type = ctx
        override val plugin: PluginHelper.this.plugin.type = PluginHelper.this.plugin


        override def extractOptions(options: Options): plugin.Options =
          PluginHelper.this.getOptions(options)

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
      override def optionsCodec: OptionCodec[Env, Error, Options] = optionsHandler.toCodec

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
      import context.given

      override def getTube(tubeName: TubeName): Comp[ArTube] =
        tubes.get(tubeName).commit
          .some
          .orElseFail(DiagnosticError.UnknownTube(tubeName))

      override def loadTube(resFactory: ResourceFactory[context.Env, context.Error])(tubeOptions: TubeOptions): ZIO[R & Scope, E, ArTube] =
        for
          plugin <- ZIO.fromEither(getPlugin(tubeOptions.loader.plugin).toRight(UnknownPlugin(tubeOptions.loader.plugin)))
          loader <- ZIO.fromEither(plugin.tubeLoaders[context.Options].get(tubeOptions.loader.name).toRight(UnknownTubeLoader(tubeOptions.loader)))

          libOptions <- ZIO.fromEither(
            loader.libOptionCodec
              .decode(resFactory)(tubeOptions.options)
              .left.map(BuildConfigParseError.apply)
          )

          tube <- loader.load(ctx)(this)(libOptions)
          _ <- ZSTM.ifSTM(tubes.contains(tube.tubeName))(
            onTrue = ZSTM.fail(DuplicateTube(tube.tubeName)),
            onFalse = tubes.put(tube.tubeName, tube),
          ).commit
        yield tube
    }


}

private[build] trait OptionMapCodec[R, E, A] {
  def encode(recorder: ResourceRecorder[R, E])(value: A): ZIO[R, E, Map[String, ESExpr]]
  def decode(resFactory: ResourceFactory[R, E])(value: ESExpr): Either[String, A]

  def toCodec: OptionCodec[R, E, A] =
    new OptionCodec[R, E, A] {
      override lazy val tags: Set[ESExprTag] =
        summon[ESExprCodec[Dictionary[ESExpr]]].tags

      override def encode(recorder: ResourceRecorder[R, E])(value: A): ZIO[R, E, ESExpr] =
        OptionMapCodec.this.encode(recorder)(value).map { map =>
          summon[ESExprCodec[Dictionary[ESExpr]]].encode(Dictionary(map))
        }

      override def decode(resFactory: ResourceFactory[R, E])(value: ESExpr): Either[String, A] =
        OptionMapCodec.this.decode(resFactory)(value)
    }
}

private[build] object BuildContextFactory {
  def make[R <: CompEnv, E >: BuildError | CompError](plugins: Map[String, Plugin[R, E]], buildConfig: BuildConfig): ZIO[R, E, BuildContextFactory[R, E]] =
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

private[build] final class BuildContextFactoryNil[R <: CompEnv, E >: BuildError | CompError] extends BuildContextFactory[R, E] {
  override type Options = EmptyTuple
  override type ExternMethodImplementation = EmptyTuple
  override type ExternFunctionImplementation = EmptyTuple
  override type ExternClassConstructorImplementation = EmptyTuple

  override given optionsHandler: OptionMapCodec[R, E, Options] with

    override def decode(resFactory: ResourceFactory[R, E])(value: ESExpr): Either[String, Options] =
      Right(EmptyTuple)

    override def encode(recorder: ResourceRecorder[R, E])(value: Options): ZIO[R, E, Map[String, ESExpr]] =
      ZIO.succeed(Map.empty)

  end optionsHandler


  override def getPluginHelper(name: String): Option[PluginHelper] = None

  override def getExternMethodImplementation(options: Options, id: String): ZIO[R, Option[E], ExternMethodImplementation] =
    ZIO.succeed(EmptyTuple)

  override def getExternFunctionImplementation(options: Options, id: String): ZIO[R, Option[E], ExternFunctionImplementation] =
    ZIO.succeed(EmptyTuple)

  override def getExternClassConstructorImplementation(options: Options, id: String): ZIO[R, Option[E], ExternClassConstructorImplementation] =
    ZIO.succeed(EmptyTuple)

}

private[build] final class BuildContextFactoryCons[R <: CompEnv, E >: BuildError | CompError]
(pluginName: String, val plugin: Plugin[R, E], val rest: BuildContextFactory[R, E]) extends BuildContextFactory[R, E] {
  override type Options = plugin.Options *: rest.Options
  override type ExternMethodImplementation = plugin.ExternMethodImplementation *: rest.ExternMethodImplementation
  override type ExternFunctionImplementation = plugin.ExternFunctionImplementation *: rest.ExternFunctionImplementation
  override type ExternClassConstructorImplementation = plugin.ExternClassConstructorImplementation *: rest.ExternClassConstructorImplementation

  override given optionsHandler: OptionMapCodec[R, E, Options] with
    override def decode(resFactory: ResourceFactory[R, E])(value: ESExpr): Either[String, Options] =
      for
        pluginOptMap <- summon[ESExprCodec[Dictionary[ESExpr]]].decode(value)
        pluginOptExpr <- pluginOptMap.dict.get(pluginName).toRight("Missing plugin options")
        pluginOpt <- plugin.optionCodec.decode(resFactory)(pluginOptExpr)
        tailOpt <- rest.optionsHandler.decode(resFactory)(value)
      yield pluginOpt *: tailOpt
    end decode

    override def encode(recorder: ResourceRecorder[R, E])(value: plugin.Options *: rest.Options): ZIO[R, E, Map[String, ESExpr]] =
      val (head *: tail) = value
      for
        headExpr <- plugin.optionCodec.encode(recorder)(head)
        tailExpr <- rest.optionsHandler.encode(recorder)(tail)
      yield tailExpr.updated(pluginName, headExpr)
    end encode
  end optionsHandler


  override def getPluginHelper(name: String): Option[PluginHelper] =
    if name == pluginName then
      Some(new PluginHelper {
        override val plugin: BuildContextFactoryCons.this.plugin.type = BuildContextFactoryCons.this.plugin
        override def getOptions(options: Options): plugin.Options =
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
          override def getOptions(options: Options): plugin.Options =
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

