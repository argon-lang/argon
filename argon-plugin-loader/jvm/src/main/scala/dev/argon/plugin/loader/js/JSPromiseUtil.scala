package dev.argon.plugin.loader.js

import dev.argon.util.ErrorWrapper
import zio.*

import scala.reflect.TypeTest
import org.graalvm.polyglot.{Context, PolyglotException, Value}

final class JSPromise[A] private (private val value: Value)

object JSPromise {
  given[A]: ValueEncoder[JSPromise[A]] with
    override def encode(value: JSPromise[A]): Value = value.value
  end given

  given[A]: ValueDecoder[JSPromise[A]] with
    override def decode(value: Value): JSPromise[A] = JSPromise(value)
  end given
}

object JSPromiseUtil {

  def runEffectToPromise[R, E, EX <: Throwable, A: ValueEncoder](a: ZIO[R, E, A])(using runtime: Runtime[R], errorWrapper: ErrorWrapper[E, EX], jsContext: Context): JSPromise[A] =
    Unsafe.unsafely {
      type CallbackFunc = ((A => Unit), (Value => Unit)) => Unit
      val callback: CallbackFunc = (resolve, reject) =>
        runtime.unsafe.run(a.foldCauseZIO(
          failure = cause => ZIO.succeed { reject(jsContext.asValue(errorWrapper.wrap(cause)).nn) },
          success = value => ZIO.succeed { resolve(value) },
        ))

      ValueDecoder[JSPromise[A]].decode(
        jsContext.eval("js", "callback => new Promise(callback)").nn
          .execute(
            ValueEncoder[CallbackFunc].encode(callback)
          ).nn
      )
    }

  def fromPromiseJS[A: ValueDecoder](a: => JSPromise[A])(using jsContext: Context): Task[A] =
    ZIO.async { onComplete =>
      val onFulfilled: A => Unit = value => onComplete(ZIO.succeed(value))
      val onRejected: Value => Unit = error => {
        val errorObj: Any =
          if error.isHostObject then
            error.asHostObject()
          else
            error

        errorObj.asInstanceOf[Matchable] match {
          case ex: Throwable => ZIO.fail(ex)
          case ex => ZIO.fail(JavaScriptException(ex))
        }
      }

      ValueEncoder[JSPromise[A]].encode(a)
        .invokeMember("then", ValueEncoder[A => Unit].encode(onFulfilled), ValueEncoder[Value => Unit].encode(onRejected))
    }

  def promiseToEffect[E, EX <: Throwable, A: ValueDecoder](a: => JSPromise[A])(using ErrorWrapper[E, EX], TypeTest[Throwable, EX], Context): IO[E, A] =
    ErrorWrapper.unwrapEffect(fromPromiseJS(a))

}
