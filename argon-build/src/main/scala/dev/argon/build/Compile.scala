package dev.argon.build

import dev.argon.io.*
import dev.argon.compiler.*
import dev.argon.compiler.tube.{ArTubeC, TubeName}
import dev.argon.options.{OptionCodec, OutputHandler, OutputInfo}
import dev.argon.plugin.*
import dev.argon.util.toml.{Toml, TomlCodec}
import zio.*
import zio.stm.*

import java.io.IOException

object Compile {
  def compile[R <: ResourceReader & ResourceWriter, E >: BuildError | CompError | IOException](
    buildConfig: Toml,
    plugins: Map[String, Plugin[E]],
  ): ZIO[R, E, Unit] =
    ZIO.scoped(
      for
        config <- ZIO.fromEither(summon[TomlCodec[BuildConfig]].decode(buildConfig))
          .tapError { error => Console.printLineError(error).orDie }
          .mapError(BuildConfigParseError.apply)

        contextFactory <- BuildContextFactory.make(plugins, config)

        _ <- ZIO.unit // Used to allow below type annotation
        context: contextFactory.ContextRefined = contextFactory.createContext

        tubeImporter <- contextFactory.createTubeImporter(context)

        resReader <- ZIO.service[ResourceReader]

        tube <- tubeImporter.loadTube(resReader)(config.tube)
        declTube <- ZIO.fromEither(tube.asDeclaration.toRight { CouldNotLoadDeclarationTube(tube.tubeName) })

        _ <- ZIO.foreachDiscard(config.libraries)(tubeImporter.loadTube(resReader))

        _ <- ZIO.logTrace(s"Writing output")
        _ <- ZIO.foreachDiscard(config.output) { case (pluginName, outputOptions) =>
          for
            pluginHelper <- ZIO.fromEither(contextFactory.getPluginHelper(pluginName).toRight(UnknownPlugin(pluginName)))
            tubeOutput <- pluginHelper.plugin.emitTube(context)(pluginHelper.adapter(context))(declTube)
            _ <- handlePluginOutput(pluginName, Seq(), outputOptions, tubeOutput, pluginHelper.plugin.outputHandler)
          yield ()
        }

      yield ()
    )


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
