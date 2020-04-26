package dev.argon.build.testrunner

import cats._
import cats.implicits._
import zio.interop.catz._
import dev.argon.build._
import java.io.IOException

import dev.argon.io.Path
import cats.data.NonEmptyList
import dev.argon.backend.{Backend, ProjectFileHandler}
import dev.argon.compiler.{CompilationError, CompilerOptions}
import zio._
import shapeless.{Id => _, Path => _, _}
import dev.argon.backend.ProjectLoader.Implicits._
import dev.argon.backend.ProjectFileHandler
import dev.argon.build._
import dev.argon.compiler.loaders.ResourceIndicator
import dev.argon.io.fileio.FileIO
import dev.argon.module.PathResourceIndicator

final class BuildTestCaseRunner(protected val backend: Backend, referencePaths: RIO[FileIO, Vector[ResourceIndicator]]) extends TestCaseRunnerCompilePhase {

  override val name: String = s"Compilation (${backend.name})"

  private implicit val dummyFileHandler: ProjectFileHandler[ZIO[BuildEnvironment, Nothing, *], ResourceIndicator] = new ProjectFileHandler[ZIO[BuildEnvironment, Nothing, *], ResourceIndicator] {
    override def loadSingleFile(file: String): ZIO[BuildEnvironment, Nothing, ResourceIndicator] =
      Path.of(file).map(PathResourceIndicator.apply)
  }


  override protected def backendOptions(compilerOptions: CompilerOptions[Id]): ZIO[BuildEnvironment, IOException, backend.BackendOptions[Id, ResourceIndicator]] =
    backend.projectLoader.loadProject[ZIO[BuildEnvironment, Nothing, *]](
      backend.inferBackendOptions(compilerOptions, backend.emptyBackendOptions)
    )


  override protected def getProgramOutput(compOutput: backend.TCompilationOutput): ZIO[BuildEnvironment, NonEmptyList[CompilationError], Either[Throwable, String]] =
    IO.succeed(Right(""))

  override def runTest(testCase: TestCase): ZIO[BuildEnvironment, Throwable, TestCaseResult] =
    referencePaths.flatMap { references =>
      compileTestCase(testCase, references)
    }

  override protected def normalizeOutput(output: String): String = ""
}
