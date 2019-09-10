package dev.argon.build.testrunner.node

import java.io.{IOException}
import java.nio.file.Path

import dev.argon.build.testrunner._
import cats._
import cats.data.{NonEmptyList, NonEmptyVector}
import cats.implicits._
import zio._
import zio.interop.catz._
import dev.argon.build.testrunner.node.ExternalApi._
import dev.argon.compiler.backend.{Backend, CompilationOutputText}
import dev.argon.compiler.{CompilationError, CompilerOptions, IOCompilation}
import dev.argon.backend.js.{JSBackend, JSBackendOptions, JSInjectCode}
import dev.argon.io.{FileIO, FilenameManip}
import dev.argon.stream.{Resource, Step, StreamTransformation}
import dev.argon.stream.{Step, StreamTransformation}
import dev.argon.build._

final class NodeTestCaseRunner(references: Vector[Path], launcher: NodeLauncher) extends TestCaseRunnerCompilePhase {

  override protected val backend: JSBackend.type = JSBackend

  override protected def backendOptions(compilerOptions: CompilerOptions[Id]): IO[IOException, JSBackendOptions[Id, Path]] =
    IO.succeed(
      JSBackendOptions[Id, Path](
        outputFile = Path.of(compilerOptions.moduleName + ".js"),
        extern = Map.empty,
        inject = JSInjectCode[Id](
          before = None,
          after = None,
        )
      )
    )

  override protected def getProgramOutput(compOutput: CompilationOutputText { val context: Backend.ContextWithComp[ZIO, BuildEnvironment, Path] }): ZIO[BuildEnvironment, NonEmptyList[CompilationError], Either[Throwable, String]] = for {
    (compiledFile, _) <- compOutput.textStream.foldLeftM("") { (a, b) => IO.succeed(a + b) }
    output <- runJSOutput(references)(compiledFile).either
  } yield output


  override def runTest(testCase: TestCase): ZIO[BuildEnvironment, Throwable, TestCaseResult] =
    compileTestCase(testCase, references)

  private def runJSOutput(files: Vector[Path])(compiledFile: String): ZIO[BuildEnvironment, Throwable, String] = for {
    referenceLibs <- files.traverse { path =>
      val libName = FilenameManip.getBasename(path)
      val libFile = Option(path.getParent).getOrElse(Path.of("")).resolve("js").resolve(libName + ".js")

      ZIO.accessM[FileIO] { _.fileIO.readAllText(libFile) }
        .map(FileInfo(libName, _))
    }

    modules = (referenceLibs :+ FileInfo(moduleName, compiledFile)).toArray

    serverFuncs <- launcher.serverFunctions
    result <- serverFuncs.executeJS(moduleName, modules)
    output <- result match {
      case ExecutionResult.Success(output) => IO.succeed(output)
      case ExecutionResult.Failure(error) => IO.fail(new RuntimeException(error + "\nCompiled output:\n" + compiledFile + "\n"))
    }
  } yield output

}
