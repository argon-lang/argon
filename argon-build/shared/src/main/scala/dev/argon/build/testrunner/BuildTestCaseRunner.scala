package dev.argon.build.testrunner

import cats._
import cats.implicits._
import zio.interop.catz._
import dev.argon.build._
import java.io.IOException

import dev.argon.io.{Path, ZipEntryInfo}
import cats.data.NonEmptyList
import dev.argon.backend.{Backend, ProjectFileHandler, ResourceAccess, ResourceReader, ResourceWriter}
import dev.argon.compiler.{Comp, CompilationError, CompilerOptions}
import zio._
import shapeless.{Id => _, Path => _, _}
import dev.argon.backend.ProjectLoader.Implicits._
import dev.argon.build._
import dev.argon.compiler.loaders.ResourceIndicator
import dev.argon.io.fileio.FileIO
import dev.argon.module.PathResourceIndicator
import dev.argon.stream.builder.Source
import scalapb.GeneratedMessage

final class BuildTestCaseRunner[I <: ResourceIndicator: Tagged](protected val backend: Backend, referencePaths: Vector[I]) extends TestCaseRunnerCompilePhase[I, ResourceReader[I] with ResourceWriter[Nothing]] {

  override val name: String = s"Compilation (${backend.name})"

  override protected def backendOptions(compilerOptions: CompilerOptions[Id]): UIO[backend.BackendOptions[Id, I]] = {
    implicit val fileHandler: ProjectFileHandler[Any, Nothing, Nothing, I] = ProjectFileHandler.nothingFileHandler
    backend.backendOptionsProjectLoader.loadProject(
      backend.inferBackendOptions(compilerOptions, backend.emptyBackendOptions)
    )
  }



  override def runTest(testCase: TestCase): URIO[ResourceReader[I] with ResourceWriter[Nothing], TestCaseActualResult] =
    compileTestCase(testCase, referencePaths)
      .use { output =>
        val emptyOutputOptions = backend.emptyOutputOptions[String]
        val outputOptionsStr = backend.inferOutputOptions(compilerOptions, emptyOutputOptions)

        import BuildTestCaseRunner.dummyFileHandler

        backend.outputOptionsProjectLoader.loadProject(outputOptionsStr)
          .flatMap { outputOptions =>
            output.write(outputOptions)
              .provideLayer(BuildTestCaseRunner.dummyWriterService)
          }
          .mapError(compilationFailureResult)
      }
      .as(TestCaseActualResult.NotExecuted)
      .catchAll(IO.succeed(_))

}

object BuildTestCaseRunner {

  private class DummyOutputPath extends ResourceIndicator {
    override def extension: String = ""
    override def show: String = "dummy-file"
  }

  private val dummyOutputPath: DummyOutputPath = new DummyOutputPath

  private implicit val dummyFileHandler: ProjectFileHandler[Any, Nothing, String, dummyOutputPath.type] =
    new ProjectFileHandler[Any, Nothing, String, dummyOutputPath.type] {
      override def loadSingleFile(file: String): ZIO[Any, Nothing, dummyOutputPath.type] =
        IO.succeed(dummyOutputPath)
    }

  private def dummyWriterService: ZLayer[ResourceWriter[Nothing], Nothing, ResourceWriter[DummyOutputPath]] =
    ZLayer.fromFunction { env =>
      val res = env.get

      new ResourceWriter.Service[DummyOutputPath] {
        override def writeToResource[X](id: DummyOutputPath)(data: Source[Comp, Chunk[Byte], X]): Comp[X] =
          data.foldLeftM(()) { (_, _) => IO.unit }.map { case (_, x) => x }

        override def zipFromEntries(entries: Source[Comp, ZipEntryInfo[Comp], Unit]): Source[Comp, Chunk[Byte], Unit] =
          res.zipFromEntries(entries)

        override def serializeProtocolBuffer(message: GeneratedMessage): Source[Comp, Chunk[Byte], Unit] =
          res.serializeProtocolBuffer(message)
      }
    }

}

