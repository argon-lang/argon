package dev.argon.backend

import dev.argon.backend.api as javaApi
import dev.argon.backend.api.HostOperations
import dev.argon.nobleidl.runtime.{ErrorType, FutureWithError, FutureWithoutError}
import dev.argon.nobleidl.runtime.graaljsInterop.{ErrorChecker, ErrorTypeAdapter, JSExecutor, CallUtil, JSAdapter}
import dev.argon.util.*
import dev.argon.util.graalext.TextDecoderPolyfill

import java.util.concurrent.{Executor as JExecutor, Executors as JExecutors}
import org.graalvm.polyglot.{Source, Context as JSContext, Value as JSValue}

import java.io.IOException
import zio.*

import java.net.URL

private[backend] object JSApiBackendLoader {

  final case class JSApiBackend(
    context: JSContext,
    executor: JSExecutor,
    backendFactory: JSValue,
  )

  def loadBackendFromJS(url: URL, exportName: String): ZIO[Scope, BackendException | IOException, JSApiBackend] =
    for
      given Runtime[Any] <- ZIO.runtime[Any]

      jExecutor <- ZIO.fromAutoCloseable(ZIO.succeed { JExecutors.newSingleThreadExecutor() })
      executor = Executor.fromJavaExecutor(jExecutor)

      javaExecutor <- ZIO.fromAutoCloseable(ZIO.succeed { JExecutors.newVirtualThreadPerTaskExecutor() })

      jsContext <- ZIO.fromAutoCloseable(ZIO.succeed {
        JSContext.newBuilder("js").nn
          .option("js.esm-eval-returns-exports", "true")
          // .option("js.text-encoding", "true")
          .option("engine.WarnInterpreterOnly", "false")
          .build()
      })

      _ <- ZIO.attempt {
        TextDecoderPolyfill.polyfill(jsContext)
      }
        .mapError(ex => BackendException("Could not load TextDecoder polyfill", ex))
        .onExecutor(executor)

      module <- ZIO.attempt {
        val source = Source.newBuilder("js", url)
          .mimeType("application/javascript+module")
          .build()

        jsContext.eval(source)
      }
        .mapError(ex => BackendException("Could not load backend module", ex))
        .onExecutor(executor)

      factory <- ZIO.succeed { module.getMember(exportName).toOption }
      factory <- ZIO.fromEither(factory.toRight(BackendException(s"Export \"$exportName\" was not found in module")))

    yield JSApiBackend(
      context = jsContext,
      executor = JSExecutor.fromExecutors(jExecutor, javaExecutor),
      backendFactory = factory,
    )

  def jsApiBackendToJava(backend: JSApiBackend): javaApi.BackendFactory =
    new javaApi.BackendFactory {
      override def create[TE, EE <: Throwable](errorType: ErrorType[TE, EE], hostOperations: HostOperations[TE]): javaApi.Backend[TE, ?, ?] =
        backend.executor.runOnJSThreadWithoutError(() => {
          val errorChecker = ErrorTypeAdapter.toJS(backend.context, backend.executor, errorType)
          CallUtil.callJSFunction(
            backend.context,
            backend.executor,
            javaApi.Backend.jsAdapter(
              JSAdapter.identity(),
              errorType,
              JSAdapter.VALUE_ADAPTER,
              JSAdapter.VALUE_ADAPTER,
            ),
            () => backend.context.eval("js", "(errorChecker, backendFactory) => backendFactory.create(errorChecker, {}, x => x)") 
          )
        }).get()
    }
    
  def loadJSApiBackend[E >: BackendException | IOException](backendName: String)(url: URL, exportName: String): ZIO[Scope, E, Backend[E]] =
    loadBackendFromJS(url, exportName)
      .map(jsApiBackendToJava)
      .flatMap(JavaApiBackendLoader.loadJavaApiBackend(backendName))

}
