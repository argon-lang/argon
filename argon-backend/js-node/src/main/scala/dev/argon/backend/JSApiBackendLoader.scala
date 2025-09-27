package dev.argon.backend

import dev.argon.util.async.ErrorWrapper
import nobleidl.core.{ErrorType, JSAdapter}
import nobleidl.sjs.core.ErrorChecker
import zio.*

import java.io.IOException
import java.net.URL

private[backend] object JSApiBackendLoader {

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

  def loadJSApiBackend[E >: BackendException | IOException](backendName: String)(factory: sjs.BackendFactory): ZIO[Scope, E, Backend[E]] =
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
