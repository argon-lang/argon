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
import dev.argon.backend.{Backend, ResourceReader, ResourceWriter}
import dev.argon.io.fileio.FileIO
import dev.argon.build._
import dev.argon.compiler.loaders.ResourceIndicator

private[testrunner] abstract class TestCaseRunnerCompilePhase[I <: ResourceIndicator: Tagged, -R <: ResourceReader[I]] extends TestCaseRunnerParsePhase[R] {

  protected val moduleName = "TestProgram"
  protected val compilerOptions = CompilerOptions[Id](
    moduleName = moduleName
  )

  protected val backend: Backend

  protected def backendOptions(compilerOptions: CompilerOptions[Id]): UIO[backend.BackendOptions[Id, I]]

  protected final def compileTestCase(testCase: TestCase, references: Vector[I]): ZManaged[R, TestCaseError, backend.TCompilationOutput] = for {
    backendOpts <- ZManaged.fromEffect(backendOptions(compilerOptions))

    output <- BuildProcess.compile(
      backend : backend.type
    )(
      parseTestCaseSource(testCase),
      references,
      compilerOptions,
      backendOpts,
    ).mapError(compilationFailureResult)

  } yield output


}
