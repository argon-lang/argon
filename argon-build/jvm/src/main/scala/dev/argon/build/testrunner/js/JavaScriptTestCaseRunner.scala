package dev.argon.build.testrunner.js

import java.io.IOException
import dev.argon.io.Path

import dev.argon.build.testrunner._
import cats._
import cats.data.{NonEmptyList, NonEmptyVector}
import cats.implicits._
import zio._
import zio.interop.catz._
import dev.argon.build.testrunner.js.ExternalApi._
import dev.argon.compiler.backend.{Backend, CompilationOutputText}
import dev.argon.compiler.{CompilationError, CompilerOptions, IOCompilation}
import dev.argon.backend.js.{JSBackend, JSBackendOptions, JSInjectCode}
import dev.argon.io.{FileIO, FilenameManip}
import dev.argon.stream.{Resource, Step, StreamTransformation}
import dev.argon.stream.{Step, StreamTransformation}
import dev.argon.build._

final class JavaScriptTestCaseRunner(referencePaths: UIO[Vector[Path]], launcher: ZManaged[BuildEnvironment, Throwable, NodeLauncher]) extends JavaScriptTestCaseRunnerBase(referencePaths) {


  override protected def executeJS(compiledFile: String)(modules: Seq[FileInfo]): ZIO[BuildEnvironment, Throwable, String] =
    launcher.use { launcher =>
      for {
        serverFuncs <- launcher.serverFunctions
        result <- serverFuncs.executeJS(moduleName, modules.toArray)
        output <- result match {
          case ExecutionResult.Success(output) => IO.succeed(output)
          case ExecutionResult.Failure(error) => IO.fail(new RuntimeException(error + "\nCompiled output:\n" + compiledFile + "\n"))
        }
      } yield output
    }


}

object JavaScriptTestCaseRunner {

  def apply(jsScriptFile: String)(references: UIO[Vector[Path]]): JavaScriptTestCaseRunner =
    new JavaScriptTestCaseRunner(references, NodeLauncher(jsScriptFile))

}
