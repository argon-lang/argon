package dev.argon.build

import dev.argon.io.*
import dev.argon.compiler.*
import dev.argon.esexpr.{ESExpr, ESExprCodec}
import dev.argon.options.{OptionDecoder, OutputHandler, OutputInfo}
import dev.argon.plugin.*
import dev.argon.plugin.platform.pluginFactories
import zio.*
import zio.stm.*

import java.io.IOException

object Compile {
  def compile[R <: ResourceReader & ResourceWriter & PluginEnv, E >: BuildError | IOException | PluginError](
    config: BuildConfig,
  ): ZIO[R, E, Unit] =
    ZIO.scoped(
      createContext[R, BuildError | IOException](config)
        .flatMap { context =>
          createLogger(context).flatMap { logger =>
            compileImpl(config, context)
              .provideSomeEnvironment[R & Scope](_.add[context.ErrorLog](logger))
          }
        }
    )

  private def createContext[R <: ResourceReader & ResourceWriter & PluginEnv, E >: BuildError | IOException]
  (config: BuildConfig)
  : ZIO[R & Scope, E, PluginContext[R, E]] =
    ZIO.foreach(config.plugins)(name => ZIO.fromEither(pluginFactories.get(name).toRight(UnknownPlugin(name))))
      .flatMap(PluginLoader.load)
      .map { pluginSet =>
        new PluginContext[R, E] {
          override val plugins: PluginSet = pluginSet
          override val implementations: Implementations {
            type ExternFunctionImplementation = plugins.externFunction.Implementation
            type FunctionReference = plugins.externFunction.Reference
          } = new Implementations {
            override type ExternFunctionImplementation = plugins.externFunction.Implementation
            override type FunctionReference = plugins.externFunction.Reference
          }
        }
      }

  private trait LogReporter {
    def reportLogs: IO[BuildError | IOException, Unit]
  }

  private def createLogger(context: Context): UIO[context.ErrorLog & LogReporter] =
    for
      errors <- Ref.make(Seq.empty[context.CompilerError])
    yield new context.ErrorLog with LogReporter {
      override def logError(error: => context.CompilerError): UIO[Unit] =
        errors.update(_ :+ error)

      override def reportLogs: IO[BuildError | IOException, Unit] =
        errors.get.flatMap { errors =>
          if errors.isEmpty then
            ZIO.unit
          else
            ZIO.foreach(errors) { e => Console.printError(e) } *>
              ZIO.fail(BuildFailed(errorCount = errors.size))

          end if
        }
    }

  private def compileImpl(config: BuildConfig, context: PluginContext[? <: ResourceReader & ResourceWriter, ? >: IOException | BuildError]): ZIO[context.Env & Scope, context.Error, Unit] =
    for
      tubeImporter <- TubeImporterImpl(context)

      resReader <- ZIO.service[ResourceReader]
      tube <- tubeImporter.loadTube(resReader)(path => ESExprCodec.ErrorPath.Keyword("build-config", "tube", ESExprCodec.ErrorPath.Keyword("tube-options", "options", path)))(config.tube)
      _ <- ZIO.foreachDiscard(config.libraries.zipWithIndex) { (lib, i) =>
        tubeImporter.loadTube(resReader)(path => ESExprCodec.ErrorPath.Keyword("build-config", "libraries", ESExprCodec.ErrorPath.Positional("list", i, ESExprCodec.ErrorPath.Keyword("tube-options", "options", path))))(lib)
      }

      _ <- ZIO.logTrace(s"Writing output")

      emitter = context.plugins.emitter[context.type]

      outputOptions <- ZIO.fromEither(
        emitter.outputOptionsDecoder[context.Error]
          .decode(resReader)(config.output.options)
          .left.map(error => BuildConfigParseError(
            ESExprCodec.DecodeError(
              error.message,
              ESExprCodec.ErrorPath.Keyword(
                "build-config",
                "output",
                ESExprCodec.ErrorPath.Keyword(
                  "output-config",
                  "options",
                  error.path,
                ),
              ),
            )
          ))
      )

      output <- emitter.emitTube(context)(tube)(outputOptions)
      _ <- handlePluginOutput(Seq(), config.output.dest, output, emitter.outputHandler)
    yield ()

  private def handlePluginOutput[R <: ResourceWriter, E >: BuildError | IOException, Output](prefix: Seq[String], outputOptions: DeepStringDict, output: Output, handler: OutputHandler[E, Output]): ZIO[R, E, Unit] =
    outputOptions match
      case DeepStringDict.Dict(map) =>
        ZIO.foreachDiscard(map.dict) { case (name, value) =>
          handlePluginOutput(prefix :+ name, value, output, handler)
        }

      case DeepStringDict.Str(value) =>
        for
          _ <- ZIO.logTrace(s"Writing output: ${prefix.mkString(".")} to $value")
          outputInfo <- ZIO.fromEither(handler.options.get(prefix).toRight(UnknownOutput(prefix)))
          _ <- outputInfo.getValue(output) match {
            case FileSystemResource.Of(resource) => ZIO.serviceWithZIO[ResourceWriter](_.write(value, resource))
            case resource: DirectoryResource[E, BinaryResource] => ZIO.serviceWithZIO[ResourceWriter](_.write(value, resource))
          }
        yield ()
    end match


}
