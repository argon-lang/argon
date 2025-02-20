package dev.argon.backend

import dev.argon.backend.api as javaApi
import dev.argon.util.async.ErrorWrapper
import nobleidl.core.{ErrorType, JavaAdapter}
import zio.*

import java.io.IOException

private[backend] object JavaApiBackendLoader {

  def javaBackendToScala[E >: BackendException | IOException, Outs](using ew: ErrorWrapper[E], rt: Runtime[Any])(backendName: String)(backend: javaApi.Backend[ew.EX, Outs]): UIO[Backend[E]] =
    ScalaApiBackendLoader.loadScalaApiBackend(
      backendName
    )(
      scalaApi.Backend.javaAdapter[ew.EX, ew.EX, Outs, Outs](
        JavaAdapter.identity,
        JavaAdapter.identity,
      ).fromJava(backend)
    )

  def loadJavaApiBackend[E >: BackendException | IOException](backendName: String)(factory: javaApi.BackendFactory): ZIO[Scope, E, Backend[E]] =
    for
      given Runtime[Any] <- ZIO.runtime[Any]
      backend <- ZIO.suspendSucceed {
        val errorContext = ErrorWrapper.Context[E]()
        import errorContext.given

        val javaBackend = createBackend(factory)
        javaBackendToScala(backendName)(javaBackend)
      }
    yield backend

  private def createBackend[E >: BackendException | IOException](factory: javaApi.BackendFactory)(using ew: ErrorWrapper[E]): javaApi.Backend[ew.EX, ?] =
    import ew.given
    val errorType = ErrorType.toJavaErrorType(summon[ErrorType[ew.EX]])

    val hostOperations = new javaApi.HostOperations[ew.EX] {
      override def errorFromIOException(ex: IOException): ew.EX =
        ew.wrap(Cause.fail(ex))
    }

    factory.create(errorType, hostOperations)
  end createBackend


}
