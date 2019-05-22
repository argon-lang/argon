package dev.argon.build.testrunner

import cats._
import cats.implicits._
import scalaz.zio.interop.catz._
import dev.argon.build._
import java.io.{File, IOException}

import cats.data.NonEmptyList
import dev.argon.compiler.{CompilationError, CompilerOptions}
import dev.argon.build.project.ProjectFileHandler
import scalaz.zio._
import shapeless.{Id => _, _}
import dev.argon.build.project.ProjectLoader.Implicits._

final class BuildTestCaseRunner(protected val backend: Backend, references: Vector[File]) extends TestCaseRunnerCompilePhase {

  private implicit val dummyFileHandler: ProjectFileHandler[UIO, File] = new ProjectFileHandler[UIO, File] {
    override def loadSingleFile(file: String): UIO[File] =
      IO.effectTotal { new File(file) }

    override def loadFileGlob(glob: String): UIO[List[File]] =
      IO.succeed(Nil)
  }


  override protected def backendOptions(compilerOptions: CompilerOptions[Id]): IO[IOException, backend.BackendOptions[Id, File]] =
    backend.projectLoader.loadProject[UIO](
      backend.inferBackendOptions(compilerOptions, backend.emptyBackendOptions)
    )


  override protected def getProgramOutput(compOutput: backend.TCompilationOutput[IO, File]): IO[NonEmptyList[CompilationError], Either[Throwable, String]] =
    IO.succeed(Right(""))

  override def runTest(testCase: TestCase): IO[Throwable, TestCaseResult] =
    compileTestCase(testCase, references)

  override protected def normalizeOutput(output: String): String = ""
}
