package dev.argon.driver

import dev.argon.util.async.ErrorWrapper
import dev.argon.backend.*
import zio.*
import dev.argon.backend.metadata.BackendMetadata
import dev.argon.backend.api as javaApi
import dev.argon.backend.scalaApi
import dev.argon.driver.api.{BackendMetadataParseException, CompilerDriver as JavaCompilerDriver}
import dev.argon.driver.api.command.DriverCommand as JavaDriverCommand
import dev.argon.driver.scalaApi.command.DriverCommand
import zio.logging.*
import nobleidl.core.{ErrorType, JavaAdapter}

import java.io.{BufferedReader, IOException, InputStreamReader}
import scala.jdk.CollectionConverters.*
import scala.compiletime.asMatchable

class CompilerDriver extends JavaCompilerDriver {
  override def parseMetadata(metadata: String): javaApi.metadata.BackendMetadata =
    toml.Toml.parse(metadata)
      .flatMap { metadata =>
        summon[toml.Codec[BackendMetadata]](metadata, defaults = Map.empty, index = 0)
      }
      .map(BackendMetadata.toApi andThen scalaApi.metadata.BackendMetadata.javaAdapter().toJava)
      .left.map { (_, message) => throw new BackendMetadataParseException(message) }
      .toTry
      .get

  override def parseCommandLineArguments(backends: java.util.List[javaApi.metadata.BackendMetadata], arguments: Array[String]): JavaDriverCommand[String, String, String, String] =
    val args = arguments.toSeq

    val parser = CompilerDriverOptions.command(
      backends
        .asScala
        .view
        .map(scalaApi.metadata.BackendMetadata.javaAdapter().fromJava andThen BackendMetadata.fromApi)
        .toSeq
    )
    
    val parsed = parser.parse(args, Map.empty)
    val driverCommand = CompilerDriverOptions.toDriverCommand(args, parsed) 

    DriverCommand.javaAdapter(
      JavaAdapter.identity,
      JavaAdapter.identity,
      JavaAdapter.identity,
      JavaAdapter.identity,
    ).toJava(driverCommand)
  end parseCommandLineArguments

  override def runCommand(options: dev.argon.driver.api.CompilerDriverOptions): Int =
    Unsafe.unsafely {
      val runtime = Runtime.default

      val logConfig = ConsoleLoggerConfig(
        format = LogFormat.colored,
        filter = LogFilter.LogLevelByNameConfig(LogLevel.Trace),
      )

      val console = new zio.Console {
        override def print(line: => Any)(using trace: Trace): IO[IOException, Unit] =
          Unsafe.unsafely {
            ZIO.attemptBlockingIO(unsafe.print(line))
          }

        override def printError(line: => Any)(using trace: Trace): IO[IOException, Unit] =
          Unsafe.unsafely {
            ZIO.attemptBlockingIO(unsafe.printError(line))
          }

        override def printLine(line: => Any)(using trace: Trace): IO[IOException, Unit] =
          Unsafe.unsafely {
            ZIO.attemptBlockingIO(unsafe.printLine(line))
          }

        override def printLineError(line: => Any)(using trace: Trace): IO[IOException, Unit] =
          Unsafe.unsafely {
            ZIO.attemptBlockingIO(unsafe.printLineError(line))
          }

        override def readLine(using trace: Trace): IO[IOException, String] =
          Unsafe.unsafely {
            ZIO.attemptBlockingIO(unsafe.readLine())
          }

        override def unsafe: UnsafeAPI = new UnsafeAPI {
          override def print(line: Any)(using unsafe: Unsafe): Unit =
            options.getStdout.print(line)

          override def printError(line: Any)(using unsafe: Unsafe): Unit =
            options.getStderr.print(line)

          override def printLine(line: Any)(using unsafe: Unsafe): Unit =
            options.getStdout.println(line)

          override def printLineError(line: Any)(using unsafe: Unsafe): Unit =
            options.getStderr.println(line)


          private val reader = new BufferedReader(new InputStreamReader(options.getStdin))
          override def readLine()(using unsafe: Unsafe): String =
            reader.readLine()
        }
      }

      runtime.unsafe.run(
        runCommandZIO(options)
          .withConsole(console)
          .provideLayer(
            (Runtime.removeDefaultLoggers >+> makePrintStreamLogger(logConfig.format.toLogger, options.getStderr).install) ++
              Runtime.enableLoomBasedExecutor ++
              Runtime.enableLoomBasedBlockingExecutor
          )
      )
    }
      .getOrElse { cause =>
        val ex = cause.failureOption
          .map { error =>
            error.asMatchable match {
              case ex: Throwable => ex
              case _ => FiberFailure(cause)
            }
          }
          .orElse {
            if cause.isInterrupted then
              Some(InterruptedException())
            else
              None
          }
          .orElse {
            cause.defects.headOption
          }
          .getOrElse {
            FiberFailure(cause)
          }

        ex.printStackTrace(options.getStderr)
        ExitCode.failure
      }
      .code

  private def runCommandZIO(options: dev.argon.driver.api.CompilerDriverOptions): ZIO[Any, Any, ExitCode] =
    val backendFactories =
      options.getBackendFactories
        .asScala
        .iterator
        .map { javaBackendFactory =>
          new BackendFactory {
            override val metadata: BackendMetadata =
              BackendMetadata.fromApi(
                scalaApi.metadata.BackendMetadata.javaAdapter().fromJava(javaBackendFactory.metadata())
              )

            override def load[E >: BackendException | IOException]: ZIO[Scope, E, Backend[E]] =
              for
                given Runtime[Any] <- ZIO.runtime[Any]
                backend <- ZIO.suspendSucceed {
                  val errorContext = ErrorWrapper.Context[E]()
                  given ew: ErrorWrapper[E] = errorContext.errorWrapper
                  import ew.given
                  val errorType = ErrorType.toJavaErrorType(summon[ErrorType[ew.EX]])

                  val hostOperations = new javaApi.HostOperations[ew.EX] {
                    override def errorFromIOException(ex: IOException): ew.EX =
                      ew.wrap(Cause.fail(ex))
                  }

                  javaBackendFactory.create(errorType, hostOperations) match {
                    case javaBackend: javaApi.Backend[ew.EX, outs] =>
                      ScalaApiBackendLoader.loadScalaApiBackend(
                        metadata.backend.name
                      )(
                        scalaApi.Backend.javaAdapter[ew.EX, ew.EX, outs, outs](
                          JavaAdapter.identity,
                          JavaAdapter.identity,
                        ).fromJava(javaBackend)
                      )
                  }
                }
              yield backend

          }
        }
        .toSeq

    ZIO.runtime[Any]
      .flatMap { runtime =>
        given Runtime[Any] = runtime

        CompilerDriverImpl.runCommand(
          DriverCommand.javaAdapter(
            scalaApi.BinaryResource.javaAdapter(JavaAdapter.identity[IOException]),
            scalaApi.DirectoryResource.javaAdapter(JavaAdapter.identity[IOException]),
            scalaApi.BinaryResourceSink.javaAdapter(JavaAdapter.identity[IOException]),
            scalaApi.DirectoryResourceSink.javaAdapter(JavaAdapter.identity[IOException]),
          ).fromJava(options.getCommand)
        )
      }

      .provideLayer(BackendProvider.liveFromFactories(backendFactories))
  end runCommandZIO

}
