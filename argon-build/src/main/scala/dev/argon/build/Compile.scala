package dev.argon.build

import dev.argon.io.*
import dev.argon.compiler.*
import dev.argon.compiler.tube.{ArTubeC, TubeName}
import dev.argon.esexpr.{ESExpr, ESExprCodec}
import dev.argon.options.{OptionDecoder, OutputHandler, OutputInfo}
import dev.argon.plugin.*
import dev.argon.plugin.platform.{AnyPluginEnv, AnyPluginError}
import dev.argon.util.toml.{Toml, TomlCodec}
import zio.*
import zio.stm.*

import java.io.IOException

object Compile {
  def compile[R <: ResourceReader & ResourceWriter & AnyPluginEnv, E >: BuildError | AnyPluginError | CompError | IOException](
    config: BuildConfig,
  ): ZIO[R, E, Unit] =
    ZIO.scoped(
      for
        pluginLoader <- PluginLoader.make[R, E](config)
        
        tubePlugin <- pluginLoader.loadInputPlugin(config.tube.loader.plugin)
        
        context = PluginContext[R, E](tubePlugin)

        tubeImporter <- TubeImporterImpl[R, E](pluginLoader)(context)

        resReader <- ZIO.service[ResourceReader]

        tube <- tubeImporter.loadTube(resReader)(config.tube)
        declTube <- ZIO.fromEither(tube.asDeclaration.toRight { CouldNotLoadDeclarationTube(tube.tubeName) })

        _ <- ZIO.foreachDiscard(config.libraries)(tubeImporter.loadTube(resReader))

        _ <- ZIO.logTrace(s"Writing output")
        
        _ <- ZIO.foreachDiscard(config.output.output) { case (pluginName, OutputConfig(options, dest)) =>
          for
            outputPluginWithAdapter <- pluginLoader.loadOutputPlugin(pluginName)(context)
            outputOptions <- ZIO.fromEither(
              outputPluginWithAdapter.plugin.outputOptionsDecoder
                .decode(resReader)(options)
                .left.map(BuildConfigParseError.apply)
            )
            tubeOutput <- outputPluginWithAdapter.plugin.emitTube(context)(outputPluginWithAdapter.adapter)(declTube)(outputOptions)
            _ <- handlePluginOutput(pluginName, Seq(), dest, tubeOutput, outputPluginWithAdapter.plugin.outputHandler)
          yield ()
        }
      yield ()
    )


  private def handlePluginOutput[R <: ResourceWriter, E >: BuildError | IOException, Output](pluginName: String, prefix: Seq[String], outputOptions: ESExpr, output: Output, handler: OutputHandler[R, E, Output]): ZIO[R, E, Unit] =
    outputOptions match
      case ESExpr.Constructed("dict", map, Seq()) =>
        ZIO.foreachDiscard(map) { case (name, value) =>
          handlePluginOutput(pluginName, prefix :+ name, value, output, handler)
        }

      case ESExpr.Str(value) =>
        for
          _ <- ZIO.logTrace(s"Writing output: ${prefix.mkString(".")} to $value")
          outputInfo <- ZIO.fromEither(handler.options.get(prefix).toRight(UnknownOutput(pluginName, prefix)))
          _ <- outputInfo.getValue(output) match {
            case FileSystemResource.Of(resource) => ZIO.serviceWithZIO[ResourceWriter](_.write(value, resource))
            case resource: DirectoryResource[R, E, BinaryResource] => ZIO.serviceWithZIO[ResourceWriter](_.write(value, resource))
          }
        yield ()

      case _ => ZIO.fail(BuildConfigParseError("Invalid type for output option. Expected string or table."))
    end match


}
