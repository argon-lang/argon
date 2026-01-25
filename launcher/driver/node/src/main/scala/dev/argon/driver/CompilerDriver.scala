package dev.argon.driver

import dev.argon.backend.metadata.BackendMetadata
import dev.argon.backend.{Backend, BackendException, BackendFactory, BackendProvider}
import dev.argon.backend.{ScalaApiBackendLoader, scalaApi, sjs}
import dev.argon.driver.CompilerDriver.parseCommandLineArguments
import dev.argon.driver.{scalaApi as dScalaApi, sjs as dsjs}
import dev.argon.util.async.{ErrorWrapper, JSPromiseUtil}
import dev.argon.util.{*, given}
import zio.*
import nobleidl.core.{ErrorType, JSAdapter}

import java.io.IOException
import scala.scalajs.js
import scala.scalajs.js.annotation.*

@JSExportTopLevel("compilerDriver")
object CompilerDriver extends dsjs.CompilerDriver {
  override def parseMetadata(metadata: String): sjs.metadata.BackendMetadata =
    toml.Toml.parse(metadata)
      .flatMap { metadata =>
        summon[toml.Codec[BackendMetadata]](metadata, defaults = Map.empty, index = 0)
      }
      .map(BackendMetadata.toApi andThen scalaApi.metadata.BackendMetadata.jsAdapter().toJS)
      .left.map { (_, message) => throw js.JavaScriptException(js.Error(message)) }
      .toTry
      .get

  override def parseCommandLineArguments(backends: js.Array[sjs.metadata.BackendMetadata], arguments: js.Array[String]): dsjs.command.DriverCommand =
    val args = arguments.toSeq

    val parser = CompilerDriverOptions.command(
      backends
        .view
        .map(scalaApi.metadata.BackendMetadata.jsAdapter().fromJS andThen BackendMetadata.fromApi)
        .toSeq
    )

    val parsed = parser.parse(args, Map.empty)
    val driverCommand = CompilerDriverOptions.toDriverCommand(args, parsed)

    dScalaApi.command.DriverCommand.jsAdapter().toJS(driverCommand)
  end parseCommandLineArguments

  override def runCommand(options: dsjs.CompilerDriverOptions): js.Promise[Int] =
    given Runtime[Any] = Runtime.default

    val backendFactories = options.backendFactories
      .view
      .map { factory =>
        new BackendFactory {
          override val metadata: BackendMetadata =
            BackendMetadata.fromApi(
              scalaApi.metadata.BackendMetadata.jsAdapter().fromJS(factory.metadata)
            )

          override def load[E >: BackendException | IOException]: ZIO[Scope, E, Backend[E]] =
            loadJSApiBackend(metadata.backend.name)(factory)
        }
      }
      .toSeq



    JSPromiseUtil.runEffectToPromiseRaw(
      CompilerDriverImpl.runCommand(dScalaApi.command.DriverCommand.jsAdapter().fromJS(options.command))
        .provideLayer(BackendProvider.liveFromFactories(backendFactories))
        .mapErrorCause[Throwable] { cause =>
          cause.failureOption match {
            case Some(ex: Throwable) => Cause.fail(ex)
            case Some(_) => Cause.fail(FiberFailure(cause))
            case None => cause.stripFailures
          }
        }
        .map { _.code }
    )
  end runCommand


  private def jsBackendToScala[E >: BackendException | IOException, Outs](using ew: ErrorWrapper[E], rt: Runtime[Any])(backendName: String)(backend: sjs.Backend[ew.EX, Outs]): UIO[Backend[E]] =
    val adapter = scalaApi.Backend.jsAdapter[ew.EX, ew.EX, Outs, Outs](
      JSAdapter.identity,
      JSAdapter.identity,
    )

    val backendScala = adapter.fromJS(backend)

    ScalaApiBackendLoader.loadScalaApiBackend(
      backendName
    )(
      backendScala
    )

  private def loadJSApiBackend[E >: BackendException | IOException](backendName: String)(factory: sjs.BackendFactory): ZIO[Scope, E, Backend[E]] =
    for
      given Runtime[Any] <- ZIO.runtime[Any]
      backend <- ZIO.suspendSucceed {
        val errorContext = ErrorWrapper.Context[E]()
        import errorContext.given

        val jsBackend = createBackend(factory)
        jsBackendToScala(backendName)(jsBackend)
      }
    yield backend

  private def createBackend[E >: BackendException | IOException](factory: sjs.BackendFactory)(using ew: ErrorWrapper[E]): sjs.Backend[ew.EX, ?] =
    import ew.given
    val errorType = ErrorType.toJSErrorChecker(summon[ErrorType[ew.EX]])

    val hostOperations = new sjs.HostOperations[ew.EX] {}

    factory.create(errorType, hostOperations, backend => backend)
  end createBackend
}
