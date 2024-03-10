package dev.argon.build

import dev.argon.compiler.*
import dev.argon.esexpr.ESExprCodec
import dev.argon.io.ResourceFactory
import dev.argon.plugin.{PluginContext, PluginEnv, PluginSet, TubeLoaderName}
import zio.*
import zio.stm.*

object TubeImporterImpl {
  def apply(ctx: PluginContext[?, ? >: BuildError]): UIO[TubeImporter & LoadTube & HasContext[ctx.type]] =
    for
      tubes <- TMap.empty[TubeName, ArTubeC & HasContext[ctx.type]].commit
    yield new TubeImporter with LoadTube {
      override val context: ctx.type = ctx
      import context.given

      override def getTube(tubeName: TubeName): Comp[ArTube] =
        tubes.get(tubeName).commit
          .some
          .orElseFail(UnknownTube(tubeName))

      override def loadTube(resFactory: ResourceFactory[context.Error])(pathMapper: ESExprCodec.ErrorPath => ESExprCodec.ErrorPath)(tubeOptions: TubeOptions): ZIO[context.Env & Scope, context.Error, ArTube] =
        for
          loader <- ZIO.fromEither(context.plugins.tubeLoaders[context.type].get(TubeLoaderName(tubeOptions.loader.plugin, tubeOptions.loader.name)).toRight(UnknownTubeLoader(tubeOptions.loader)))

          libOptions <- ZIO.fromEither(
            loader.libOptionDecoder
              .decode(resFactory)(tubeOptions.options)
              .left.map(error => BuildConfigParseError(ESExprCodec.DecodeError(error.message, pathMapper(error.path))))
          )

          tube <- loader.load(ctx)(this)(libOptions)
          _ <- ZSTM.ifSTM(tubes.contains(tube.name))(
            onTrue = ZSTM.fail(DuplicateTube(tube.name)),
            onFalse = tubes.put(tube.name, tube),
          ).commit
        yield tube
    }
}
