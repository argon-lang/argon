package dev.argon.backend.platforms.js

import dev.argon.backend.*


import dev.argon.io.TextResource
import dev.argon.io.DirectoryResource
import dev.argon.vm.resource.VmIrResourceContext
import dev.argon.compiler.*

import java.util.concurrent.{Executors as JExecutors, Executor as JExecutor}

import org.graalvm.polyglot.{Context as GraalContext, *}


import zio.*
import zio.stream.*

import dev.argon.util.async.ErrorWrapper
import dev.argon.util.async.JavaExecuteIO
import scala.collection.mutable.ArrayBuffer
import dev.argon.io.DirectoryEntry
import dev.argon.io.Resource
import cats.data.NonEmptySeq

trait JSBackendPlatformSpecific {
  self: JSBackend.type =>

  def codegen(
    context: BackendContext
  )(
    vmIrResContext: VmIrResourceContext & HasContext[context.type]
  )(
    options: Options[context.Error],
    program: vmIrResContext.VmIrResource[context.Error],
    libraries: Map[TubeName, vmIrResContext.VmIrResource[context.Error]],
  ): context.Comp[Output[context.Error]] =

    import context.Comp
    
    class JSCodegenImpl(jsContext: GraalContext, jExecutor: JExecutor, executor: Executor)(using runtime: Runtime[context.Env]) {

      val errorContext = ErrorWrapper.Context[context.Error]
      import errorContext.given


      def attemptJSContextTask[A](f: => A): Task[A] =
        ZIO.attempt { f }
          .onExecutor(executor)

      def attemptJSContext[A](f: => A): Comp[A] =
        JavaExecuteIO.runJava(f)
          .onExecutor(executor)

      def runAsPromise(io: Comp[Value]): Value =
        jsContext.eval("js", "callback => new Promise(callback)").nn.execute(((resolve, reject) => {
          Unsafe.unsafely {
            runtime.unsafe.fork(io)
              .unsafe.addObserver {
                case Exit.Success(a) => jExecutor.execute(() => resolve.execute(a))
                case Exit.Failure(cause) => jExecutor.execute(() => reject.execute(errorContext.ContextException(cause)))
              }
          }
        }) : PromiseCallback)

      def promiseToTask(promise: => Value): Task[Value] =
        ZIO.async { register =>
          jExecutor.execute(() => {
            promise.invokeMember("then",
              ((result: Value) => register(ZIO.succeed(result))) : ThenCallback,
              ((result: Value) => 
                if result.isException() then
                  register(ZIO.attempt { throw result.throwException() })
                else
                  register(ZIO.fail(errorContext.ContextException(Cause.fail(JavaScriptException(result.toString())))))
              ) : ThenCallback,
            )
          })
        }

      def promiseToIO(promise: => Value): Comp[Value] =
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
        val a = jsContext.eval("js", "n => new Uint8Array()").nn.execute(c.length)

        for i <- 0 until c.length do
          a.setArrayElement(i, c.byte(i))

        a
      end toUint8Array


      def streamToAsyncIterable(stream: ZStream[context.Env, context.Error, Value]): Value =
        val streamPullPromise = runAsPromise(
          for
            scope <- Scope.make
            pullAction <- stream.channel.toPull.provideSomeEnvironment[context.Env](_ ++ ZEnvironment(scope))
            streamPull <- attemptJSContext(jsContext.asValue(new StreamPull {
              override def pull(): Value =
                runAsPromise(pullAction.flatMap {
                  case Left(_) => attemptJSContext(jsContext.asValue(null))
                  case Right(items) => attemptJSContext(toJSArray(items))
                })

              override def close(): Value =
                runAsPromise(scope.close(Exit.unit) *> attemptJSContext(jsContext.eval("js", "undefined")))
            }))
          yield streamPull
        )

        jsContext.eval("js",
          """async function*(streamPullPromise) {
            |    const streamPull = await streamPullPromise;
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
            |}""".stripMargin
        ).execute(streamPullPromise)

      end streamToAsyncIterable

      def asyncIterableToStream(asyncIterableThunk: => Value): Stream[context.Error, Value] =
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
              attemptJSContextTask {
                val res = asyncIterable.invokeMember("next")
                if res.getMember("done").asBoolean() then
                  ZIO.fail(None)
                else
                  val value = res.getMember("value").nn
                  ZIO.succeed(Chunk(value))
                end if
              }.asSomeError.flatten
            )
          )
        ).viaFunction(ErrorWrapper.unwrapStream)


      def runCodegen(input: CodegenInput): ZStream[context.Env, context.Error, ModuleCodegenResult] =
        asyncIterableToStream {
          val source = Source.newBuilder("js", classOf[JSBackend.type].getResource("js-backend.js")).nn
              .mimeType("application/javascript+module")
              .build()

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

    def runCodegen: ZStream[context.Env, context.Error, ModuleCodegenResult] =
      ZStream.unwrapScoped(
        for
          given Runtime[context.Env] <- ZIO.runtime[context.Env]

          jExecutor <- ZIO.fromAutoCloseable(ZIO.succeed { JExecutors.newSingleThreadExecutor() })
          executor = Executor.fromJavaExecutor(jExecutor)

          jsContext <- ZIO.fromAutoCloseable(ZIO.succeed {
            GraalContext.newBuilder("js").nn
              .option("js.esm-eval-returns-exports", "true")
              .build()
          })

          codegenImpl = JSCodegenImpl(jsContext, jExecutor, executor)

        yield codegenImpl.runCodegen(new CodegenInput {
          override def getTubeMapping(): Value =
            jsContext.eval("js", "[]")


          override def getTubeInput(): TubeInput =
            new TubeInput.Encoded {
              override def data(): Value =
                codegenImpl.streamToAsyncIterable(
                  program.asBytes.chunks
                    .mapZIO(chunk => codegenImpl.attemptJSContext(codegenImpl.toUint8Array(chunk)))
                )
            }

          override def getExterns(): Value =
            codegenImpl.streamToAsyncIterable(
              ZStream.fromIterable(options.externs)
                .mapZIO { externRes =>
                  for
                    sourceCode <- externRes.asText.run(ZSink.mkString)
                    value <- codegenImpl.attemptJSContext {
                      jsContext.asValue(ExternsInfo(sourceCode, externRes.fileName.getOrElse("externs.js")))
                    }
                  yield value
                }
            )
        })
      )

    def modulesAsDirectory(stream: Stream[context.Error, ModuleCodegenResult]): DirectoryResource[context.Error, TextResource] =
      new DirectoryResource[context.Error, TextResource] with Resource.WithoutFileName {
        override def contents: Stream[context.Error, DirectoryEntry[context.Error, TextResource]] =
          stream.map { moduleRes =>
            val res = new TextResource.Impl[context.Error] with Resource.WithoutFileName {
              override def asText: Stream[context.Error, String] = ZStream(moduleRes.sourceCode)
            }

            DirectoryEntry(moduleRes.moduleFilePath.init, moduleRes.moduleFilePath.last, res)
          }
      }

    for
      env <- ZIO.environment[context.Env]
    yield JSOutput(
      sourceCode = modulesAsDirectory(
        runCodegen.provideEnvironment(env)
      ),
    )
  end codegen

  private final case class ModuleCodegenResult(
    moduleFilePath: NonEmptySeq[String],
    sourceCode: String,
  )
  

}
