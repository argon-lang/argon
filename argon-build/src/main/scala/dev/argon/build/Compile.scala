package dev.argon.build

import dev.argon.io.{BinaryResource, DirectoryResource, ResourceFactory, ResourceWriter}
import dev.argon.compiler.*
import dev.argon.compiler.tube.{ArTubeC, TubeName}
import dev.argon.options.{OptionDecoder, OutputHandler, OutputInfo}
import dev.argon.plugin.*
import dev.argon.util.toml.{Toml, TomlCodec}
import zio.*
import zio.stm.*

import java.io.IOException

object Compile {
  def compile[R <: ResourceFactory & ResourceWriter, E >: BuildError | CompError | IOException](
    buildConfig: Toml,
    plugins: Map[String, Plugin[R, E]],
  ): ZIO[R, E, Unit] =
    ZIO.scoped(
      for
        config <- ZIO.fromEither(summon[TomlCodec[BuildConfig]].decode(buildConfig))
          .tapError { error => Console.printLineError(error).orDie }
          .mapError(BuildConfigParseError.apply)
        tubeOptions = config.tube


        runtime <- ZIO.runtime[Any]

        context <- ZIO.succeed {
          new Context with LoadTube {
            override type Env = R
            override type Error = E

            private val tubes =
              Unsafe.unsafe {
                runtime.unsafe.run(TMap.empty[TubeName, ArTubeC with HasContext[this.type]].commit).getOrThrow()
              }

            override def getTube(tubeName: TubeName): Comp[ArTubeC with HasContext[this.type]] =
              tubes.get(tubeName).commit
                .some
                .orElseFail(DiagnosticError.UnknownTube(tubeName))


            def loadTube
            (loaderOptions: TubeLoaderOptions)
            (tubeOptions: Toml)
            : ZIO[R & Scope, E, ArTubeC with HasContext[this.type]] =
              for
                plugin <- ZIO.fromEither(plugins.get(loaderOptions.plugin).toRight(UnknownPlugin(loaderOptions.plugin)))
                loader <- ZIO.fromEither(plugin.tubeLoaders.get(loaderOptions.loader).toRight(UnknownTubeLoader(loaderOptions)))

                options <- plugin.optionDecoder[E]
                  .decode(config.plugin.get(loaderOptions.plugin).flatMap(_.options).getOrElse(Toml.Table.empty))
                  .mapError(BuildConfigParseError.apply)

                libOptions <- loader.libOptionDecoder[E]
                  .decode(tubeOptions)
                  .mapError(BuildConfigParseError.apply)

                tube <- loader.load(this)(options, libOptions)
                _ <- ZSTM.ifSTM(tubes.contains(tube.tubeName))(
                  onTrue = ZSTM.fail(DuplicateTube(tube.tubeName)),
                  onFalse = tubes.put(tube.tubeName, tube),
                ).commit
              yield tube

          }
        }

        loaderOptions = tubeOptions.loader.getOrElse(TubeLoaderOptions(plugin = "source", loader = "buildspec"))
        tube <- context.loadTube(loaderOptions)(tubeOptions.options)

        _ <- ZIO.foreachDiscard(config.libraries) { lib =>
          context.loadTube(lib.loader)(lib.options)
        }

        _ <- ZIO.logTrace(s"Writing output")
        _ <- ZIO.foreachDiscard(config.plugin) { case (pluginName, pluginConfig) =>
          for
            plugin <- ZIO.fromEither(plugins.get(pluginName).toRight(UnknownPlugin(pluginName)))
            pluginOptions <- plugin.optionDecoder[E].decode(pluginConfig.options.getOrElse(Toml.Table.empty)).mapError(BuildConfigParseError.apply)
            tubeOutput <- plugin.backend.emitTube(context)(pluginOptions)(tube)
            _ <- handlePluginOutput(pluginName, Seq(), pluginConfig.output.getOrElse(Toml.Table.empty), tubeOutput, plugin.outputHandler)
          yield ()
        }

      yield ()
    )

  private trait LoadTube:
    self: Context =>

    def loadTube
    (loaderOptions: TubeLoaderOptions)
    (tubeOptions: Toml)
    : ZIO[Env & Scope, Error, ArTubeC with HasContext[this.type]]

  end LoadTube

  private def handlePluginOutput[R <: ResourceWriter, E >: BuildError | IOException, Output](pluginName: String, prefix: Seq[String], outputOptions: Toml, output: Output, handler: OutputHandler[R, E, Output]): ZIO[R, E, Unit] =
    outputOptions match
      case Toml.Table(map) =>
        ZIO.foreachDiscard(map) { case (name, value) =>
          handlePluginOutput(pluginName, prefix :+ name, value, output, handler)
        }

      case Toml.String(value) =>
        for
          _ <- ZIO.logTrace(s"Writing output: ${prefix.mkString(".")} to $value")
          outputInfo <- ZIO.fromEither(handler.options.get(prefix).toRight(UnknownOutput(pluginName, prefix)))
          _ <- outputInfo.getValue(output) match {
            case resource: BinaryResource[R, E] => ZIO.serviceWithZIO[ResourceWriter](_.write(value, resource))
            case resource: DirectoryResource[R, E, BinaryResource] => ZIO.serviceWithZIO[ResourceWriter](_.write(value, resource))
          }
        yield ()

      case _ => ZIO.fail(BuildConfigParseError("Invalid type for output option. Expected string or table."))
    end match


}
