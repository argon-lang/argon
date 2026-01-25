package dev.argon.driver

import dev.argon.util.async.ErrorWrapper
import dev.argon.backend.*
import zio.*
import dev.argon.backend.metadata.BackendMetadata
import dev.argon.backend.api as javaApi
import dev.argon.backend.scalaApi
import dev.argon.driver.api.{CompilerDriver as JavaCompilerDriver, BackendMetadataParseException}
import dev.argon.driver.api.command.DriverCommand as JavaDriverCommand
import dev.argon.driver.scalaApi.command.DriverCommand
import zio.logging.*
import nobleidl.core.{ErrorType, JavaAdapter}

import java.io.IOException
import java.nio.file.Path
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

  override def parseCommandLineArguments(backends: java.util.List[javaApi.metadata.BackendMetadata], arguments: Array[String]): JavaDriverCommand[String] =
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

    DriverCommand.javaAdapter(JavaAdapter.identity).toJava(driverCommand)
  end parseCommandLineArguments

  override def runCommand(options: dev.argon.driver.api.CompilerDriverOptions): Int =
    Unsafe.unsafely {
      val runtime = Runtime.default
      runtime.unsafe.run(
        runCommandZIO(options)
          .provideLayer(
            (Runtime.removeDefaultLoggers >+> consoleLogger(ConsoleLoggerConfig(
              format = LogFormat.colored,
              filter = LogFilter.LogLevelByNameConfig(LogLevel.Trace),
            ))) ++
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

        ex.printStackTrace()
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

    CompilerDriverImpl.runCommand(DriverCommand.javaAdapter(JavaAdapter.identity).fromJava(options.getCommand))
      .provideLayer(BackendProvider.liveFromFactories(backendFactories))
  end runCommandZIO

}
