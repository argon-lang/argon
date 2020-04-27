package dev.argon.build.testrunner.js

import java.io.IOException

import dev.argon.io.Path
import dev.argon.build.testrunner._
import cats._
import cats.data.{EitherT, NonEmptyList, NonEmptyVector}
import cats.implicits._
import dev.argon.backend.{CompilationOutputText, ResourceAccess}
import zio._
import zio.interop.catz._
import dev.argon.compiler.{CompilationError, CompilerOptions, ErrorList}
import dev.argon.backend.js.{JSBackend, JSBackendOptions, JSInjectCode}
import dev.argon.io.fileio.FileIO
import dev.argon.build._
import dev.argon.compiler.loaders.ResourceIndicator
import dev.argon.module.PathResourceIndicator

abstract class JavaScriptTestCaseRunnerBase extends TestCaseRunnerExecutionPhase[ResourceAccess with JSModuleLoad] {

  override val name: String = "JavaScript Execution"
  override protected val backend: JSBackend.type = JSBackend

  override protected def backendOptions(compilerOptions: CompilerOptions[Id]): UIO[JSBackendOptions[Id, ResourceIndicator]] =
    Path.of(compilerOptions.moduleName + ".js").map { outputPath =>
      JSBackendOptions[Id, ResourceIndicator](
        outputFile = PathResourceIndicator(outputPath),
        extern = Map.empty,
        inject = JSInjectCode[Id](
          before = None,
          after = None,
        )
      )
    }

  override protected def getProgramOutput(compOutput: CompilationOutputText): ZIO[JSModuleLoad, TestCaseError, String] =
    for {
      (compiledFile, _) <- compOutput.textStream.foldLeftM("") { (a, b) => IO.succeed(a + b) }.mapError(compilationFailureResult)
      output <- runJSOutput(references)(compiledFile).mapError(executionFailureResult)
    } yield output

  private def runJSOutput(files: Vector[ResourceIndicator])(compiledFile: String): RIO[JSModuleLoad, String] = for {
    referenceLibs <- files.traverse { id =>
      for {
        (libName, content) <- ZIO.accessM[JSModuleLoad] { _.get.loadJSForArgonModule(id) }
      } yield FileInfo(libName, content)
    }

    modules = (referenceLibs :+ FileInfo(moduleName, compiledFile))

    output <- executeJS(compiledFile)(modules)
  } yield output

  protected def executeJS(compiledFile: String)(modules: Seq[FileInfo]): Task[String]

}

