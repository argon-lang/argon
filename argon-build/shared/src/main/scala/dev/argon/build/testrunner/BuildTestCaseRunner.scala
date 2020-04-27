package dev.argon.build.testrunner

import cats._
import cats.implicits._
import zio.interop.catz._
import dev.argon.build._
import java.io.IOException

import dev.argon.io.Path
import cats.data.NonEmptyList
import dev.argon.backend.{Backend, ProjectFileHandler, ResourceAccess}
import dev.argon.compiler.{CompilationError, CompilerOptions}
import zio._
import shapeless.{Id => _, Path => _, _}
import dev.argon.backend.ProjectLoader.Implicits._
import dev.argon.build._
import dev.argon.compiler.loaders.ResourceIndicator
import dev.argon.io.fileio.FileIO
import dev.argon.module.PathResourceIndicator

final class BuildTestCaseRunner(protected val backend: Backend, referencePaths: Vector[ResourceIndicator]) extends TestCaseRunnerCompilePhase[ResourceAccess] {

  override val name: String = s"Compilation (${backend.name})"



  private implicit val dummyFileHandler: ProjectFileHandler[UIO, ResourceIndicator] = new ProjectFileHandler[UIO, ResourceIndicator] {
    override def loadSingleFile(file: String): UIO[ResourceIndicator] =
      Path.of(file).map(PathResourceIndicator.apply)
  }


  override protected def backendOptions(compilerOptions: CompilerOptions[Id]): UIO[backend.BackendOptions[Id, ResourceIndicator]] =
    backend.projectLoader.loadProject[UIO](
      backend.inferBackendOptions(compilerOptions, backend.emptyBackendOptions)
    )


  override def runTest(testCase: TestCase): URIO[ResourceAccess, TestCaseActualResult] =
    compileTestCase(testCase, referencePaths)
      .use { output => output.write.mapError(compilationFailureResult) }
      .as(TestCaseActualResult.NotExecuted)
      .catchAll(IO.succeed(_))

}
