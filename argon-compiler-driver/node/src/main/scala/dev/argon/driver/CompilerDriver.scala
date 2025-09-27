package dev.argon.driver

import dev.argon.backend.BackendProvider
import dev.argon.util.async.JSPromiseUtil
import dev.argon.util.{*, given}
import zio.{Cause, FiberFailure, Runtime, Unsafe}

import scala.scalajs.js
import scala.scalajs.js.annotation.*

object CompilerDriver extends CompilerDriverInterface {
  override def runCommand(options: CompilerDriverJSOptions): js.Promise[Int] =
    given Runtime[Any] = Runtime.default
    
    JSPromiseUtil.runEffectToPromiseRaw(
      BackendLoaderUtils.loadAllBackends(options.pluginDirectories.toSeq)
        .flatMap { backendFactories =>
          CompilerDriverImpl.runCommand(options.arguments.toSeq)
            .provideLayer(BackendProvider.liveFromFactories(backendFactories))
        }
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
}
