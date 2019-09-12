package dev.argon.build.testrunner.js

import java.io.IOException
import dev.argon.io.Path

import dev.argon.build.testrunner._
import cats._
import cats.data.{NonEmptyList, NonEmptyVector}
import cats.implicits._
import zio._
import zio.interop.catz._
import dev.argon.compiler.backend.{Backend, CompilationOutputText}
import dev.argon.compiler.{CompilationError, CompilerOptions, IOCompilation}
import dev.argon.backend.js.{JSBackend, JSBackendOptions, JSInjectCode}
import dev.argon.io.{FileIO, FilenameManip, JSIOException}
import dev.argon.build._

final class JavaScriptTestCaseRunner(references: UIO[Vector[Path]]) extends JavaScriptTestCaseRunnerBase(references) {

  override protected def executeJS(compiledFile: String)(modules: Seq[FileInfo]): ZIO[BuildEnvironment, Throwable, String] = ???

}

object JavaScriptTestCaseRunner {

  def apply(references: UIO[Vector[Path]]): JavaScriptTestCaseRunner =
    new JavaScriptTestCaseRunner(references)

}
