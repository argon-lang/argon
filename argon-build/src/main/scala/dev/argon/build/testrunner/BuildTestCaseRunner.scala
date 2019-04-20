package dev.argon.build.testrunner

import scalaz.{Scalaz, _}
import Scalaz._
import scalaz.zio.interop.scalaz72._
import dev.argon.build._
import java.io.File

import dev.argon.compiler.CompilerOptions
import scalaz._
import Scalaz._
import dev.argon.build.project.ProjectFileHandler
import scalaz.zio.IO
import scalaz.zio.interop.scalaz72._
import shapeless.{Id => _, _}
import dev.argon.build.project.ProjectLoader.Implicits._

final class BuildTestCaseRunner(protected val backend: Backend, references: Vector[File]) extends TestCaseRunnerCompilePhase {

  private implicit val dummyFileHandler: ProjectFileHandler[IO, File] = new ProjectFileHandler[IO, File] {
    override def loadSingleFile(file: String): IO[Throwable, File] =
      IO.effect { new File(file) }

    override def loadFileGlob(glob: String): IO[Throwable, List[File]] =
      IO.succeed(Nil)
  }

  override protected def backendOptions(compilerOptions: CompilerOptions[Id]): IO[Throwable, backend.BackendOptions[Id, File]] =
    backend.projectLoader.loadProject(
      backend.inferBackendOptions(compilerOptions, backend.emptyBackendOptions)
    )

  override protected def getProgramOutput(compOutput: backend.TCompilationOutput[IO[Throwable, +?], File]): IO[Throwable, String] =
    IO.succeed("")

  override protected def normalizeOutput(output: String): String = ""

  override def runTest(testCase: TestCase): IO[Throwable, TestCaseResult] =
    compileTestCase(testCase, references)
}
