package dev.argon.backend.backends.js

import dev.argon.backend.*
import dev.argon.vm.resource.VmIrResource
import dev.argon.compiler.*
import dev.argon.io.*

import zio.*
import zio.stream.*
import dev.argon.util.async.AsyncIterableTools
import dev.argon.util.async.ErrorWrapper
import dev.argon.util.async.AsyncIterableTools.AsyncIterable
import java.io.IOException

import scala.scalajs.js

trait JSBackendPlatformSpecific[E >: BackendException | IOException] {
  self: JSBackend[E] =>

  val codeGenerator: CodeGenerator.LibraryCodeGenerator[E, Output] { type Options = JSOptions } =
    new CodeGenerator.LibraryCodeGenerator[E, Output] {
      override type Options = JSOptions
      
      override def codegen(
        options: Options,
        program: VmIrResource[E],
        libraries: Map[TubeName, VmIrResource[E]],
      ): ZIO[Scope, E, Output] =

        val errorContext = ErrorWrapper.Context[E]
        import errorContext.given
  
        def runCodegen(using Runtime[Any]): Stream[E, ModuleCodegenResult] =
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
  
        def modulesAsDirectory(stream: Stream[E, ModuleCodegenResult]): DirectoryResource[E, TextResource] =
          new DirectoryResource[E, TextResource] with Resource.WithoutFileName {
            override def contents: Stream[E, DirectoryEntry[E, TextResource]] =
              stream.map { moduleRes =>
                val res = new TextResource.Impl[E] with Resource.WithoutFileName {
                  override def asText: Stream[E, String] = ZStream(moduleRes.sourceCode)
                }
  
                DirectoryEntry(moduleRes.moduleFilePath.init.toSeq, moduleRes.moduleFilePath.last, res)
              }
          }
  
        for
          given Runtime[Any] <- ZIO.runtime
        yield JSOutput(
          sourceCode = modulesAsDirectory(
            runCodegen
          ),
        )
      end codegen
    }

}
