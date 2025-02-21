package dev.argon.compiler_tests

import dev.argon.compiler.TubeName
import zio.*
import zio.stream.*
import dev.argon.util.*
import dev.argon.io.*
import dev.argon.tube as t
import dev.argon.tube.resource.TubeResourceContext
import dev.argon.vm
import dev.argon.compiler.*
import dev.argon.source.ArgonSourceCodeResource
import dev.argon.build.{LogReporter, Compile}
import dev.argon.io.PathLike
import dev.argon.build.GenerateIR
import dev.argon.vm.resource.VmIrResource

trait ArgonLibraryProvider {
  def getLibrary(tubeName: TubeName): ESExprDecodedBinaryStreamResource[TestError, t.TubeFileEntry]
  def getIrLibrary(tubeName: TubeName): VmIrResource[TestError]
}

object ArgonLibraryProvider {
  def live: ZLayer[Any, Nothing, ArgonLibraryProvider] =
    ZLayer.fromZIO(
      for
        libraryStore <- MemoCacheStore.make[Any, TestError, TubeName, Chunk[t.TubeFileEntry]]
        irLibraryStore <- MemoCacheStore.make[Any, TestError, TubeName, Chunk[vm.TubeFileEntry]]
      yield new ArgonLibraryProvider {
        override def getLibrary(tubeName: TubeName): ESExprDecodedBinaryStreamResource[TestError, t.TubeFileEntry] =
          getLibraryImpl(tubeName, referencingLibraries = Set.empty)

        private def getLibraryImpl(tubeName: TubeName, referencingLibraries: Set[TubeName]): ESExprDecodedBinaryStreamResource[TestError, t.TubeFileEntry] =
          new ESExprDecodedBinaryStreamResource.Impl[TestError, t.TubeFileEntry] with Resource.WithoutFileName {

            override def decoded: Stream[TestError, t.TubeFileEntry] =
              if referencingLibraries.contains(tubeName) then
                ZStream.fail(TestException(s"Circular dependency for library: $tubeName"))
              else
                ZStream.unwrap(
                  libraryStore.usingCreate(tubeName) { tubeName =>
                    for
                      library <- ZIO.fromEither(
                        ArgonLibraries.allLibraries
                          .get(tubeName)
                          .toRight { TestException(s"Unknown library: $tubeName") }
                      )

                      encodedLib <- compileLibrary(library, referencingLibraries).runCollect
                    yield encodedLib
                  }.map { entries =>
                    ZStream.fromChunk(entries)
                  }
                )
          }
          
        override def getIrLibrary(tubeName: TubeName): VmIrResource[TestError] =
          new VmIrResource[TestError] with ESExprDecodedBinaryStreamResource.Impl[TestError, vm.TubeFileEntry] with Resource.WithoutFileName {

            override def decoded: Stream[TestError, vm.TubeFileEntry] =
              ZStream.unwrap(
                irLibraryStore.usingCreate(tubeName) { tubeName =>
                  for
                    library <- ZIO.fromEither(
                      ArgonLibraries.allLibraries
                        .get(tubeName)
                        .toRight { TestException(s"Unknown library: $tubeName") }
                    )

                    encodedLib <- genIrLibrary(library).runCollect
                  yield encodedLib
                }.map { entries =>
                  ZStream.fromChunk(entries)
                }
              )
          }


        private def compileLibrary(library: ArgonLibraryInfo, referencingLibraries: Set[TubeName]): Stream[TestError, t.TubeFileEntry] =
          ZStream.unwrapScoped {
            val ctx = Context.Impl[ErrorLog & LogReporter, TestError]
            for
              tubeResContext <- TubeResourceContext.make(ctx)

              compile = new Compile {
                override val context: ctx.type = ctx

                override val tubeResourceContext: tubeResContext.type =
                  tubeResContext

                import tubeResourceContext.TubeResource

                override def tubeName: TubeName = library.tubeName

                private def tubeDir: PathLike =
                  PathLike.fromString(s"libraries/${tubeName.encode}")

                override def inputDir: DirectoryResource[context.Error, ArgonSourceCodeResource] =
                  PathUtil.directoryResource(PathLike.join(tubeDir, "src"))
                    .decode[ArgonSourceCodeResource]

                override def referencedTubes(using TubeImporter & HasContext[ctx.type]): Seq[TubeResource[context.Error]] =
                  library.references.toSeq.map { refLibName =>
                    getLibraryImpl(refLibName, referencingLibraries = referencingLibraries + library.tubeName)
                      .decode[TubeResource]
                  }
              }

              buildOutput <- compile.compile()
            yield buildOutput.tube.decoded ++ ZStream.fromZIO(ZIO.serviceWithZIO[LogReporter](_.failOnErrors)).drain
          }.provideLayer(LogReporter.live)



        private def genIrLibrary(library: ArgonLibraryInfo): Stream[TestError, vm.TubeFileEntry] =
          ZStream.unwrapScoped {
            val ctx = Context.Impl[ErrorLog & LogReporter, TestError]
            for
              tubeResContext <- TubeResourceContext.make(ctx)

              genir = new GenerateIR {
                override val context: ctx.type = ctx

                override val tubeResourceContext: tubeResContext.type =
                  tubeResContext

                import tubeResourceContext.TubeResource

                override def inputTube(using TubeImporter & HasContext[ctx.type]): TubeResource[context.Error] =
                  getLibrary(library.tubeName).decode[TubeResource]

                override def referencedTubes(using TubeImporter & HasContext[ctx.type]): Seq[TubeResource[context.Error]] =
                  library.references.toSeq.map { refLibName =>
                    getLibrary(refLibName)
                      .decode[TubeResource]
                  }
              }

              buildOutput <- genir.compile()
            yield buildOutput.tube.decoded ++ ZStream.fromZIO(ZIO.serviceWithZIO[LogReporter](_.failOnErrors)).drain
          }.provideLayer(LogReporter.live)

      }
    )
}
