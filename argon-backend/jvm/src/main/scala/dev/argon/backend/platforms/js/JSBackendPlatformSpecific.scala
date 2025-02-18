package dev.argon.backend.platforms.js

import dev.argon.backend.*


import dev.argon.io.TextResource
import dev.argon.io.DirectoryResource
import dev.argon.vm.resource.VmIrResource
import dev.argon.compiler.*

import java.util.concurrent.{Executors as JExecutors, Executor as JExecutor}

import org.graalvm.polyglot.{Context as GraalContext, *}
import org.graalvm.polyglot.proxy.{ProxyExecutable, ProxyObject}


import zio.*
import zio.stream.*

import dev.argon.util.async.ErrorWrapper
import dev.argon.util.async.JavaExecuteIO
import scala.collection.mutable.ArrayBuffer
import dev.argon.io.DirectoryEntry
import dev.argon.io.Resource
import cats.data.NonEmptySeq
import java.io.IOException

import scala.jdk.CollectionConverters.*
import dev.argon.util.graalext.TextDecoderPolyfill

trait JSBackendPlatformSpecific[E >: BackendException | IOException] {
  self: JSBackend[E] =>

  def codegen(
    options: Options,
    program: VmIrResource[E],
    libraries: Map[TubeName, VmIrResource[E]],
  ): ZIO[Scope, E, Output] =
    
    class JSCodegenImpl(jsContext: GraalContext, jExecutor: JExecutor, executor: Executor)(using runtime: Runtime[Any]) {

      private val errorContext = ErrorWrapper.Context[E]
      import errorContext.given


      def attemptJSContextTask[A](f: => A): Task[A] =
        ZIO.attempt { f }
          .onExecutor(executor)

      def attemptJSContext[A](f: => A): IO[E, A] =
        JavaExecuteIO.runJava(f)
          .onExecutor(executor)

      def runAsPromise(io: IO[E, Value]): Value =
        jsContext.eval("js", "callback => new Promise(callback)").nn.execute(
          new ProxyExecutable {
            override def execute(arguments: Value*): AnyRef =
              arguments match {
                case Seq(resolve, reject) =>
                  Unsafe.unsafely {
                    runtime.unsafe.fork(io)
                      .unsafe.addObserver {
                        case Exit.Success(a) => jExecutor.execute(() => resolve.execute(a))
                        case Exit.Failure(cause) => jExecutor.execute(() => reject.execute(errorContext.ContextException(cause)))
                      }
                  }
                  jsContext.asValue("js", "undefined")

                case _ =>
                  throw RuntimeException("Invalid arguments for promise executor.")
              }
          }
        )

      def promiseToTask(promise: => Value): Task[Value] =
        ZIO.async[Any, Throwable, Value] { register =>
          jExecutor.execute(() => {
            promise.invokeMember("then",
              new ProxyExecutable {
                override def execute(arguments: Value*): AnyRef =
                  arguments match {
                    case Seq(result) =>
                      register(ZIO.succeed(result))

                    case _ => 
                      register(ZIO.die(RuntimeException("Invalid arguments for promise resolve callback.")))
                  }

                  jsContext.eval("js", "undefined")
                end execute
              },
              new ProxyExecutable {
                override def execute(arguments: Value*): AnyRef =
                  arguments match {
                    case Seq(result) =>
                      if result.isException() then
                        val ex =
                          try result.throwException()
                          catch {
                            case ex: Throwable => ex
                          }

                        register(ZIO.fail(ex))
                      else
                        register(ZIO.fail(errorContext.ContextException(Cause.fail(JavaScriptException(result.toString())))))

                    case _ => 
                      register(ZIO.die(RuntimeException("Invalid arguments for promise reject callback.")))
                  }

                  jsContext.eval("js", "undefined")
                end execute
              },
            )
          })
        }

      def promiseToIO(promise: => Value): IO[E, Value] =
        ErrorWrapper.unwrapEffect(
          promiseToTask(promise)
        )
        
          

      def toJSArray(s: Seq[Value]): Value =
        val a = jsContext.eval("js", "[]")

        for item <- s do
          a.invokeMember("push", item)

        a
      end toJSArray

      def fromJSArray(a: Value): Seq[Value] =
        val builder = ArrayBuffer[Value]()
        for i <- 0L until a.getArraySize() do
          builder += a.getArrayElement(i)
        builder.toSeq
      end fromJSArray

      def toUint8Array(c: Chunk[Byte]): Value =
        val a = jsContext.eval("js", "n => new Uint8Array(n)").nn.execute(c.length)

        for i <- 0 until c.length do
          a.setArrayElement(i, c.byte(i))

        a
      end toUint8Array


      def streamToAsyncIterable(stream: Stream[E, Value]): Value =
        val streamPullPromiseFunc = new ProxyExecutable {
          override def execute(arguments: Value*): AnyRef =
            runAsPromise(
            for
              scope <- Scope.make
              pullAction <- stream.channel.toPull.provideEnvironment(ZEnvironment(scope))
              streamPull <- attemptJSContext(
                jsContext.asValue(ProxyObject.fromMap(Map(
                  "pull" -> new ProxyExecutable {
                    override def execute(arguments: Value*): AnyRef =
                      runAsPromise(pullAction.flatMap {
                        case Left(_) => attemptJSContext(jsContext.asValue(null))
                        case Right(items) => attemptJSContext(toJSArray(items))
                      })
                  },

                  "close" -> new ProxyExecutable {
                    override def execute(arguments: Value*): AnyRef =
                      runAsPromise(scope.close(Exit.unit) *>
                        attemptJSContext(jsContext.eval("js", "undefined")))
                  },
                ).asJava))
              )
            yield streamPull
          )
        }

        jsContext.eval("js",
          """(async function*(streamPullPromiseFunc) {
            |    const streamPull = await streamPullPromiseFunc();
            |    try {
            |        for(;;) {
            |            const items = await streamPull.pull();
            |            if(items === null) {
            |                break;
            |            }
            |            yield* items;
            |        }
            |    }
            |    finally {
            |        await streamPull.close();
            |    }
            |})""".stripMargin
        ).execute(streamPullPromiseFunc)

      end streamToAsyncIterable

      def asyncIterableToStream(asyncIterableThunk: => Value): Stream[E, Value] =
        ZStream.unwrapScoped(
          for
            asyncIterable <- attemptJSContextTask { asyncIterableThunk }.orDie
            isDone <- Ref.make(false)
            _ <- ZIO.scopeWith(_.addFinalizer(
              attemptJSContextTask {
                if asyncIterable.hasMember("return") then
                  asyncIterable.invokeMember("return")
              }.whenZIODiscard(isDone.get).orDie
            ))
          yield (
            ZStream.repeatZIOChunkOption(
              promiseToTask { asyncIterable.invokeMember("next") }
              .flatMap { res =>
                attemptJSContextTask {
                  if res.getMember("done").asBoolean() then
                    ZIO.fail(None)
                  else
                    val value = res.getMember("value").nn
                    ZIO.succeed(Chunk(value))
                  end if
                }
              }.asSomeError.flatten
            )
          )
        ).viaFunction(ErrorWrapper.unwrapStream)


      def runCodegen(): Stream[E, ModuleCodegenResult] =
        asyncIterableToStream {
          val source = Source.newBuilder("js", classOf[JSBackend[?]].getResource("js-backend.js")).nn
              .mimeType("application/javascript+module")
              .build()

          val input = ProxyObject.fromMap(Map(
            "tubeMapping" -> jsContext.eval("js", "[]"),

            "tubeInput" -> ProxyObject.fromMap(Map(
              "type" -> "ir-encoded",
              "data" -> new ProxyExecutable {
                override def execute(arguments: Value*): AnyRef =
                  streamToAsyncIterable(
                    program.asBytes.chunks
                      .mapZIO(chunk => attemptJSContext(toUint8Array(chunk)))
                  )
              },
            ).asJava),

            "getExterns" -> new ProxyExecutable {
              override def execute(arguments: Value*): AnyRef =
                streamToAsyncIterable(
                  ZStream.fromIterable(options.externs)
                    .mapZIO { externRes =>
                      for
                        sourceCode <- externRes.asText.run(ZSink.mkString)
                        value <- attemptJSContext {
                          jsContext.asValue(ExternsInfo(sourceCode, externRes.fileName.getOrElse("externs.js")))
                        }
                      yield value
                    }
                )
            }
          ).asJava)

          jsContext.eval(source).getMember("codegen").execute(input)
        }
          .mapZIO { externsInfo =>
            attemptJSContext {
              ModuleCodegenResult(
                moduleFilePath = NonEmptySeq.fromSeqUnsafe(fromJSArray(externsInfo.getMember("moduleFilePath")).map(_.asString())),
                sourceCode = externsInfo.getMember("sourceCode").asString().nn,
              )
            }
          }


    }

    def runCodegen: Stream[E, ModuleCodegenResult] =
      ZStream.unwrapScoped(
        for
          given Runtime[Any] <- ZIO.runtime[Any]

          jExecutor <- ZIO.fromAutoCloseable(ZIO.succeed { JExecutors.newSingleThreadExecutor() })
          executor = Executor.fromJavaExecutor(jExecutor)

          jsContext <- ZIO.fromAutoCloseable(ZIO.succeed {
            GraalContext.newBuilder("js").nn
              .allowHostAccess(HostAccess.EXPLICIT)
              .option("js.esm-eval-returns-exports", "true")
              // .option("js.text-encoding", "true")
              .option("engine.WarnInterpreterOnly", "false")
              .build()
          })

          codegenImpl = JSCodegenImpl(jsContext, jExecutor, executor)

          _ <- codegenImpl.attemptJSContextTask {
            TextDecoderPolyfill.polyfill(jsContext)
          }.orDie

        yield codegenImpl.runCodegen()
      )

    def modulesAsDirectory(stream: Stream[E, ModuleCodegenResult]): DirectoryResource[E, TextResource] =
      new DirectoryResource[E, TextResource] with Resource.WithoutFileName {
        override def contents: Stream[E, DirectoryEntry[E, TextResource]] =
          stream.map { moduleRes =>
            val res = new TextResource.Impl[E] with Resource.WithoutFileName {
              override def asText: Stream[E, String] = ZStream(moduleRes.sourceCode)
            }

            DirectoryEntry(moduleRes.moduleFilePath.init, moduleRes.moduleFilePath.last, res)
          }
      }

    for
      env <- ZIO.environment[Any]
    yield JSOutput(
      sourceCode = modulesAsDirectory(
        runCodegen
      ),
    )
  end codegen

  private final case class ModuleCodegenResult(
    moduleFilePath: NonEmptySeq[String],
    sourceCode: String,
  )
  

}
