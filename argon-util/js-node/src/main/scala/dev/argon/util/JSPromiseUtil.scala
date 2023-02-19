package dev.argon.util

import zio.*

import scala.reflect.TypeTest
import scala.scalajs.js

object JSPromiseUtil {

  def runEffectToPromise[R, E, EX <: Throwable, A](a: ZIO[R, E, A])(using runtime: Runtime[R], errorWrapper: ErrorWrapper[E, EX]): js.Promise[A] =
    Unsafe.unsafely {
      runtime.unsafe.run(ErrorWrapper.wrapEffect(a).toPromiseJS) match {
        case Exit.Success(value) => value
        case Exit.Failure(cause) => throw errorWrapper.wrap(cause)
      }
    }

  def promiseToEffect[E, EX <: Throwable, A](a: => js.Promise[A])(using ErrorWrapper[E, EX], TypeTest[Throwable, EX]): IO[E, A] =
    ErrorWrapper.unwrapEffect(ZIO.fromPromiseJS(a))

}
