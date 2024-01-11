package dev.argon.build

import dev.argon.compiler.{Context, HasContext, DiagnosticError}
import dev.argon.compiler.tube.{ArTubeC, TubeImporter, TubeName}
import dev.argon.io.ResourceFactory
import dev.argon.plugin.platform.{AnyPluginEnv, AnyPluginError}
import zio.*
import zio.stm.*

object TubeImporterImpl {
  def apply[R <: AnyPluginEnv, E >: AnyPluginError | BuildError]
  (loader: PluginLoader[R, E])
  (ctx: Context { type Env = R; type Error = E; type Options = loader.platformPlugins.Options })
  : UIO[TubeImporter & LoadTube & HasContext[ctx.type]] =
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
          plugin <- loader.loadInputPlugin(tubeOptions.loader.plugin)
          loader <- ZIO.fromEither(plugin.tubeLoaders[context.Options].get(tubeOptions.loader.name).toRight(UnknownTubeLoader(tubeOptions.loader)))

          libOptions <- ZIO.fromEither(
            loader.libOptionDecoder
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
