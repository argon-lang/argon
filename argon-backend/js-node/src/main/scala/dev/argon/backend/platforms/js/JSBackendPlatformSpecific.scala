package dev.argon.backend.platforms.js

import dev.argon.backend.*
import dev.argon.vm.resource.VmIrResourceContext
import dev.argon.compiler.*
import dev.argon.io.*

import zio.*
import zio.stream.*
import dev.argon.util.async.AsyncIterableTools
import dev.argon.util.async.ErrorWrapper
import dev.argon.util.async.AsyncIterableTools.AsyncIterable

import scala.scalajs.js

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

    val errorContext = ErrorWrapper.Context[context.Error]
    import errorContext.given

    def runCodegen(using Runtime[context.Env]): Stream[context.Error, ModuleCodegenResult] =
      AsyncIterableTools.asyncIterableToZStream(
        JSBackendModule.codegen(
          new CodegenInput {
            override val tubeMapping: js.Array[TubeMapping] = js.Array()
            
            override val tubeInput: TubeInput = new TubeInput.Ir {
              override val `type`: "ir" = "ir"
              override def entries(): AsyncIterable[dev.argon.vm.sjs.TubeFileEntry] =
                AsyncIterableTools.zstreamToAsyncIterable(
                  program.decoded
                    .map(dev.argon.vm.TubeFileEntry.jsAdapter().toJS)
                )

            }

            override def getExterns(): AsyncIterable[ExternsInfo] =
              AsyncIterableTools.zstreamToAsyncIterable(
                ZStream.fromIterable(options.externs)
                  .mapZIO { externRes =>
                    for
                      srcCode <- externRes.asText.run(ZSink.mkString)
                    yield new ExternsInfo {
                      override val sourceCode: String = srcCode
                      override val sourceFile: String = externRes.fileName.getOrElse("externs.js")
                    }
                  }
              )
            
          }
        )
      )

    def modulesAsDirectory(stream: Stream[context.Error, ModuleCodegenResult]): DirectoryResource[context.Error, TextResource] =
      new DirectoryResource[context.Error, TextResource] with Resource.WithoutFileName {
        override def contents: Stream[context.Error, DirectoryEntry[context.Error, TextResource]] =
          stream.map { moduleRes =>
            val res = new TextResource.Impl[context.Error] with Resource.WithoutFileName {
              override def asText: Stream[context.Error, String] = ZStream(moduleRes.sourceCode)
            }

            DirectoryEntry(moduleRes.moduleFilePath.init.toSeq, moduleRes.moduleFilePath.last, res)
          }
      }

    for
      env <- ZIO.environment[context.Env]
      given Runtime[context.Env] <- ZIO.runtime
    yield JSOutput(
      sourceCode = modulesAsDirectory(
        runCodegen
      ),
    )
  end codegen

}
