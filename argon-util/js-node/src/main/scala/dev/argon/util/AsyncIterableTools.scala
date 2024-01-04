package dev.argon.util

import zio.*
import zio.stream.*

import scala.reflect.TypeTest
import scala.scalajs.js.annotation.{JSGlobal, JSName}
import scalajs.js

object AsyncIterableTools {

  @js.native
  @JSGlobal("Symbol")
  private object SymbolGlobal extends js.Any {
    val asyncIterator: js.Symbol = js.native
  }

  sealed trait IteratorResult[+T, +TReturn] extends js.Any {
    val done: js.UndefOr[Boolean]
    val value: T | TReturn
  }

  trait IteratorReturnResult[TReturn] extends IteratorResult[Nothing, TReturn] {
    override val done: true
    override val value: TReturn
  }

  object IteratorReturnResult {
    def apply[TReturn](value: TReturn): IteratorReturnResult[TReturn] =
      new js.Object with IteratorReturnResult[TReturn] {
        override val done: true = true
        override val value: TReturn = value
      }
  }

  trait IteratorYieldResult[T] extends IteratorResult[T, Nothing] {
    override val done: js.UndefOr[false]
    override val value: T
  }

  object IteratorYieldResult {
    def apply[T](value: T): IteratorYieldResult[T] =
      new js.Object with IteratorYieldResult[T] {
        override val done: js.UndefOr[false] = false
        override val value: T = value
      }
  }

  extension[T, TReturn] (res: IteratorResult[T, TReturn])
    def toEither: Either[TReturn, T] =
      if res.done.getOrElse(false) then
        Left(res.asInstanceOf[IteratorReturnResult[TReturn]].value)
      else
        Right(res.asInstanceOf[IteratorYieldResult[T]].value)
  end extension


  trait AsyncIterable[+T] extends js.Any {
    @JSName(SymbolGlobal.asyncIterator)
    def asyncIterator(): AsyncIterator[T]
  }

  trait AsyncIterator[+T] extends js.Any {
    def next(): js.Promise[IteratorResult[T, Any]]
  }

  trait AsyncIteratorReturn[+T] extends AsyncIterator[T] {
    def `return`(): js.Promise[IteratorResult[T, Any]]
  }

  def iteratorHasReturn[T](iter: AsyncIterator[T]): Option[AsyncIteratorReturn[T]] =
    if iter.asInstanceOf[js.Dynamic].selectDynamic("return").asInstanceOf[js.UndefOr[Any]].isDefined then
      Some(iter.asInstanceOf[AsyncIteratorReturn[T]])
    else
      None



  def zstreamToAsyncIterable[R, E, EX <: Throwable, T](stream: ZStream[R, E, T])(using runtime: Runtime[R], errorWrapper: ErrorWrapper[E, EX]): AsyncIterable[T] =
    new js.Object with AsyncIterable[T] {
      @JSName(SymbolGlobal.asyncIterator)
      override def asyncIterator(): AsyncIterator[T] =
        new js.Object with AsyncIteratorReturn[T] {

          private var scope: Scope.Closeable | Null = null
          private var pull: ZIO[R, Option[E], Chunk[T]] | Null = null
          private var buffer: Chunk[T] = Chunk.empty

          private def getPull: ZIO[R, E, ZIO[R, Option[E], Chunk[T]]] =
            Nullable(pull).toOption.fold(
              for
                scope <- Scope.make
                pull <- scope.extend(stream.toPull)
                _ <- ZIO.succeed {
                  this.scope = scope
                  this.pull = pull
                }
              yield pull
            )(ZIO.succeed(_))

          override def next(): js.Promise[IteratorResult[T, Any]] =
            JSPromiseUtil.runEffectToPromise(nextImpl)

          private def consumeFromBuffer: ZIO[R, E, Option[T]] =
            ZIO.succeed {
              buffer match {
                case h +: t =>
                  buffer = t
                  Some(h)
                case _ => None
              }
            }

          private def readNextBuffer: ZIO[R, E, Boolean] =
            getPull
              .flatMap(_.unsome)
              .tap { nextBuff =>
                ZIO.foreachDiscard(nextBuff)(nextBuff => ZIO.succeed { buffer = nextBuff })
              }
              .map(_.isDefined)

          private def nextImpl: ZIO[R, E, IteratorResult[T, Any]] =
            consumeFromBuffer.flatMap {
              case Some(a) => ZIO.succeed(IteratorYieldResult(a))
              case None =>
                readNextBuffer.flatMap {
                  case true => nextImpl
                  case false =>
                    ZIO.foreachDiscard(Nullable(scope).toOption)(_.close(Exit.unit))
                      .as(IteratorReturnResult(()))
                }
            }


          private def closeScope(): js.Promise[IteratorResult[T, Any]] =
            Nullable(scope).toOption.fold(
              js.Promise.resolve(IteratorReturnResult(()))
            ) { scope => JSPromiseUtil.runEffectToPromise(scope.close(Exit.unit).as(IteratorReturnResult(()))) }


          override def `return`(): js.Promise[IteratorResult[T, Any]] =
            closeScope()
        }
    }

  def asyncIterableToZStreamRaw[T](iterable: => AsyncIterable[T]): Stream[Throwable, T] =
    ZStream.fromPull(
      for
        needReturn <- Ref.make(false)
        iteratorMemo <- (
          needReturn.set(true) *>
            ZIO.attempt {
              iterable.asyncIterator()
            }
          ).memoize
        _ <- Scope.addFinalizer(iteratorMemo.flatMap { iterator =>
          ZIO.foreachDiscard(iteratorHasReturn(iterator)) { iterator => ZIO.fromPromiseJS(iterator.`return`()) }
        }.orDie)
      yield (
        iteratorMemo
          .flatMap { iterator => ZIO.fromPromiseJS(iterator.next()) }
          .asSomeError
          .flatMap { res =>
            res.toEither match {
              case Left(_) => ZIO.fail(None)
              case Right(a) => ZIO.succeed(Chunk(a))
            }
          }
        )
    )

  def asyncIterableToZStream[E, EX <: Throwable, T](iterable: => AsyncIterable[T])(using ErrorWrapper[E, EX], TypeTest[Throwable, EX]): Stream[E, T] =
    ErrorWrapper.unwrapStream(asyncIterableToZStreamRaw(iterable))

}
