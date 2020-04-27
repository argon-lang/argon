package dev.argon.build.testrunner

import java.io.IOException

import dev.argon.io.Path
import dev.argon.build.BuildProcess
import dev.argon.compiler._
import dev.argon.compiler.core.ModuleDescriptor
import cats._
import cats.implicits._
import zio._
import cats.data.NonEmptyList
import dev.argon.backend.{Backend, ProjectFileHandler, ResourceAccess}
import dev.argon.io.fileio.FileIO
import dev.argon.build._
import dev.argon.compiler.loaders.ResourceIndicator

private[testrunner] trait TestCaseRunnerCompilePhase[-R <: ResourceAccess] extends TestCaseRunnerParsePhase[R] {

  protected val moduleName = "TestProgram"

  protected val backend: Backend

  protected def backendOptions(compilerOptions: CompilerOptions[Id]): UIO[backend.BackendOptions[Id, ResourceIndicator]]

  protected final def compileTestCase(testCase: TestCase, references: Vector[ResourceIndicator]): ZManaged[ResourceAccess, TestCaseError, backend.TCompilationOutput] = for {
    parsedSource <- ZManaged.fromEffect(parseTestCaseSource(testCase))

    compilerOptions = CompilerOptions[Id](
      moduleName = moduleName
    )

    backendOpts <- ZManaged.fromEffect(backendOptions(compilerOptions))

    output <- BuildProcess.compile(
      backend : backend.type
    )(
      parsedSource,
      references,
      compilerOptions,
      backendOpts,
    ).mapError(compilationFailureResult)

  } yield output


}
