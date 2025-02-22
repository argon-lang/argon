package dev.argon.backend

import dev.argon.backend.api as javaApi
import dev.argon.backend.api.HostOperations
import dev.argon.nobleidl.runtime.{ErrorType, FutureWithError, FutureWithoutError}
import dev.argon.nobleidl.runtime.graaljsInterop.{CallUtil, ErrorChecker, ErrorTypeAdapter, JSAdapter, JSExecutor}
import dev.argon.util.*
import dev.argon.util.graalext.{TextDecoderPolyfill, TextEncoderPolyfill}

import java.util.concurrent.{Executor as JExecutor, Executors as JExecutors}
import org.graalvm.polyglot.{HostAccess, Source, Context as JSContext, Value as JSValue}

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

      zioExecutor <- ZIO.executor

      jsContext <- ZIO.fromAutoCloseable(ZIO.succeed {
        JSContext.newBuilder("js")
          .allowHostClassLookup(className => className == "dev.argon.backend.backends.js.GraalJavaScriptExecutor")
          .allowHostAccess(HostAccess.EXPLICIT)
//          .option("js.load", "false")
//          .option("js.print", "false")
          .option("js.esm-eval-returns-exports", "true")
          // .option("js.text-encoding", "true")
          .option("engine.WarnInterpreterOnly", "false")
          .build()
      })
        .onExecutor(executor)
        .onExecutor(zioExecutor)

      _ <- ZIO.attempt {
        jsContext.eval(Source.newBuilder("js", classOf[JSApiBackendLoader.type].getResource("polyfill.js")).build())
        TextDecoderPolyfill.polyfill(jsContext)
        TextEncoderPolyfill.polyfill(jsContext)
      }
        .mapError(ex => BackendException("Could not load polyfills", ex))
        .onExecutor(executor)
        .onExecutor(zioExecutor)

      module <- ZIO.attempt {
        val source = Source.newBuilder("js", url)
          .mimeType("application/javascript+module")
          .build()

        jsContext.eval(source)
      }
        .mapError(ex => BackendException("Could not load backend module", ex))
        .onExecutor(executor)
        .onExecutor(zioExecutor)

      factory <- ZIO.succeed { module.getMember(exportName).toOption }
        .onExecutor(executor)
        .onExecutor(zioExecutor)

      factory <- ZIO.fromEither(factory.toRight(BackendException(s"Export \"$exportName\" was not found in module")))

    yield JSApiBackend(
      context = jsContext,
      executor = JSExecutor.fromExecutors(jExecutor, javaExecutor),
      backendFactory = factory,
    )

  def jsApiBackendToJava(backend: JSApiBackend): javaApi.BackendFactory =
    new javaApi.BackendFactory {
      override def create[TE, EE <: Throwable](errorType: ErrorType[TE, EE], hostOperations: HostOperations[TE]): javaApi.Backend[TE, ?] =
        backend.executor.runOnJSThreadWithoutError(() => {
          val errorChecker = ErrorTypeAdapter.toJS(backend.context, backend.executor, errorType)
          val jsBackend = backend.context.eval("js", "(errorChecker, backendFactory) => backendFactory.create(errorChecker, {}, x => x)").execute(errorChecker, backend.backendFactory)

          javaApi.Backend.jsAdapter(
            JSAdapter.identity(),
            errorType,
            JSAdapter.VALUE_ADAPTER,
          ).fromJS(backend.context, backend.executor, jsBackend)
        }).get()
    }
    
  def loadJSApiBackend[E >: BackendException | IOException](backendName: String)(url: URL, exportName: String): ZIO[Scope, E, Backend[E]] =
    loadBackendFromJS(url, exportName)
      .map(jsApiBackendToJava)
      .flatMap(JavaApiBackendLoader.loadJavaApiBackend(backendName))

}
