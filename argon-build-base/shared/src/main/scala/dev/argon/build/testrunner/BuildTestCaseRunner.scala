package dev.argon.build.testrunner

import cats._
import cats.implicits._
import zio.interop.catz.core._
import dev.argon.build._
import java.io.IOException

import dev.argon.io.{Path, ZipEntryInfo}
import cats.data.NonEmptyList
import dev.argon.backend.{Backend, ResourceAccess, ResourceWriter}
import dev.argon.compiler.{Comp, CompilationError, ErrorList}
import zio._
import zio.stream._
import shapeless.{Id => _, Path => _, _}
import dev.argon.build._
import dev.argon.build.testrunner.BuildTestCaseRunner.{DummyOutputPath, dummyOutputPath}
import dev.argon.build.testrunner.TestCaseRunnerCompilePhase.TestCompileResource
import dev.argon.compiler.loaders.{ResourceIndicator, ResourceReader}
import dev.argon.compiler.options.{CompilerOptions, OptionsFileHandler}
import dev.argon.io.fileio.FileIO
import dev.argon.module.PathResourceIndicator
import dev.argon.stream.builder.Source
import scalapb.GeneratedMessage

final class BuildTestCaseRunner[I <: ResourceIndicator: Tag](protected val backend: Backend, referencePaths: Vector[I]) extends TestCaseRunnerCompilePhase[I, ResourceReader[I] with ResourceWriter[Nothing]] {

  override val name: String = s"Compilation (${backend.name})"

  override protected def backendOptions: Task[backend.BackendOptions[Id, TestCompileResource[I]]] = {
    implicit val fileHandler: OptionsFileHandler[Any, Nothing, Nothing, TestCompileResource[I]] = OptionsFileHandler.nothingFileHandler

    IO.fromEither(
      backend.backendOptions.inferDefaults(backend.backendOptions.empty)
    )
      .mapError { field => new RuntimeException("Missing field in backend options: " + field.name) }
      .flatMap(backend.backendOptions.optionsLoader.loadOptions(_))
  }



  override def runTest(testCase: TestCase): URIO[ResourceReader[I] with ResourceWriter[Nothing], TestCaseActualResult] =
    compileTestCase(testCase, referencePaths)
      .use { output =>
        val emptyOutputOptions = backend.testOutputOptions[DummyOutputPath](dummyOutputPath)

        output.write(emptyOutputOptions)
          .provideLayer(BuildTestCaseRunner.dummyWriterService)
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

  private def dummyWriterService: ZLayer[ResourceWriter[Nothing], Nothing, ResourceWriter[DummyOutputPath]] =
    ZLayer.fromFunction { env =>
      val res = env.get

      new ResourceWriter.Service[DummyOutputPath] {
        override def writeToResource(id: DummyOutputPath)(data: Stream[ErrorList, Byte]): Comp[Unit] =
          data.runDrain

        override def zipFromEntries(entries: Stream[ErrorList, ZipEntryInfo[Any, ErrorList]]): Stream[ErrorList, Byte] =
          res.zipFromEntries(entries)

        override def serializeProtocolBuffer(message: GeneratedMessage): Stream[ErrorList, Byte] =
          res.serializeProtocolBuffer(message)
      }
    }

}

