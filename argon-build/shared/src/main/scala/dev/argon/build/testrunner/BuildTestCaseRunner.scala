package dev.argon.build.testrunner

import cats._
import cats.implicits._
import zio.interop.catz._
import dev.argon.build._
import java.io.{File, IOException}

import cats.data.NonEmptyList
import dev.argon.compiler.{CompilationError, CompilerOptions}
import zio._
import shapeless.{Id => _, _}
import dev.argon.compiler.backend.ProjectLoader.Implicits._
import dev.argon.compiler.backend.{Backend, ProjectFileHandler}
import dev.argon.build._

final class BuildTestCaseRunner(protected val backend: Backend, references: Vector[File]) extends TestCaseRunnerCompilePhase {

  private implicit val dummyFileHandler: ProjectFileHandler[ZIO[BuildEnvironment, Nothing, ?], File] = new ProjectFileHandler[ZIO[BuildEnvironment, Nothing, ?], File] {
    override def loadSingleFile(file: String): ZIO[BuildEnvironment, Nothing, File] =
      IO.effectTotal { new File(file) }

    override def loadFileGlob(glob: String): ZIO[BuildEnvironment, Nothing, List[File]] =
      IO.succeed(Nil)
  }


  override protected def backendOptions(compilerOptions: CompilerOptions[Id]): ZIO[BuildEnvironment, IOException, backend.BackendOptions[Id, File]] =
    backend.projectLoader.loadProject[ZIO[BuildEnvironment, Nothing, ?]](
      backend.inferBackendOptions(compilerOptions, backend.emptyBackendOptions)
    )


  override protected def getProgramOutput(compOutput: backend.TCompilationOutput { val context: Backend.ContextWithComp[ZIO, BuildEnvironment, File] }): ZIO[BuildEnvironment, NonEmptyList[CompilationError], Either[Throwable, String]] =
    IO.succeed(Right(""))

  override def runTest(testCase: TestCase): ZIO[BuildEnvironment, Throwable, TestCaseResult] =
    compileTestCase(testCase, references)

  override protected def normalizeOutput(output: String): String = ""
}
