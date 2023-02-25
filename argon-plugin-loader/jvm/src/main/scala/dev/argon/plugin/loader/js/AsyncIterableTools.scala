package dev.argon.plugin.loader.js

import dev.argon.util.{*, given}
import zio.*
import zio.direct.*
import zio.stream.*

import scala.reflect.TypeTest
import org.graalvm.polyglot.{Value, Context as JSContext}

object AsyncIterableTools {

  def zstreamToAsyncIterable[R, E, EX <: Throwable, A: ValueEncoder](stream: ZStream[R, E, A])(using runtime: Runtime[R], errorWrapper: ErrorWrapper[E, EX], jsContext: JSContext): AsyncIterable[A] =
    type CloseScopeFunc = () => JSPromise[Unit]
    type PullFunc = () => JSPromise[JSArray[A] | Null]
    type CreatePullFunc = () => JSPromise[(CloseScopeFunc, PullFunc)]
    val createPull: CreatePullFunc = () =>
      JSPromiseUtil.runEffectToPromise[R, E, EX, (CloseScopeFunc, PullFunc)](
        for
          scope <- Scope.make
          pull <- scope.extend(stream.toPull)
        yield (
          () => JSPromiseUtil.runEffectToPromise[R, E, EX, Unit](
            scope.close(Exit.unit)
          ),
          () => JSPromiseUtil.runEffectToPromise[R, E, EX, JSArray[A] | Null](pull.unsome.map {
            case Some(chunk) => JSArray.fromChunk(chunk)
            case None => null
          })
        )
      )

    val createIterableCode =
      """
      createPull => {
        [Symbol.asyncIterator]() {
          return (async function* (pull) {
            const [closeScope, pull] = await createPull();
            try {
              while(true) {
                const items = await pull();
                if(items === null) {
                  break;
                }

                for(const item : items) {
                  yield item;
                }
              }
            }
            finally {
              await closeScope();
            }
          })
        }
      }
    """

    ValueDecoder[AsyncIterable[A]].decode(jsContext.eval("js", createIterableCode).nn.execute(ValueEncoder[CreatePullFunc].encode(createPull)).nn)
  end zstreamToAsyncIterable

  def asyncIterableToZStream[E, EX <: Throwable, A: ValueDecoder](iterable: => AsyncIterable[A])(using errorWrapper: ErrorWrapper[E, EX], tt: TypeTest[Throwable, EX], jsContext: JSContext): Stream[E, A] =
    ZStream.fromPull(
      for
        needReturn <- Ref.make(false)
        iteratorMemo <- (
          needReturn.set(true) *>
          ZIO.attempt {
            jsContext.eval("js", "i => i[Symbol.asyncIterator]()").nn.execute(iterable).nn
          }
        ).memoize
        _ <- Scope.addFinalizer(iteratorMemo.flatMap[Any, Throwable, Unit] { iterator =>
          JSPromiseUtil.fromPromiseJS {
            ValueDecoder[JSPromise[Unit]].decode(jsContext.eval("js", "async i => { if(i.return) await i.return(); } ").nn.execute(iterator).nn)
          }
        }.orDie)
      yield (
        ErrorWrapper.unwrapEffect(iteratorMemo)
          .flatMap { iterator =>
            JSPromiseUtil.promiseToEffect { ValueDecoder[JSPromise[Value]].decode(iterator.invokeMember("next").nn) }
          }
          .asSomeError
          .flatMap { res =>
            if res.getMember("done").nn.asBoolean() then
              ZIO.fail(None)
            else
              ZIO.succeed(Chunk(ValueDecoder[A].decode(res.getMember("value").nn)))
          }
      )
    )

}
