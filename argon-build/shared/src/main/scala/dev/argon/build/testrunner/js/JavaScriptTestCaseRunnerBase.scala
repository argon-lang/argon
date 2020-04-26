package dev.argon.build.testrunner.js

import java.io.IOException

import dev.argon.io.Path
import dev.argon.build.testrunner._
import cats._
import cats.data.{EitherT, NonEmptyList, NonEmptyVector}
import cats.implicits._
import dev.argon.backend.CompilationOutputText
import zio._
import zio.interop.catz._
import dev.argon.compiler.{CompilationError, CompilerOptions, ErrorList}
import dev.argon.backend.js.{JSBackend, JSBackendOptions, JSInjectCode}
import dev.argon.io.fileio.FileIO
import dev.argon.build._
import dev.argon.compiler.loaders.ResourceIndicator
import dev.argon.module.PathResourceIndicator

abstract class JavaScriptTestCaseRunnerBase(referencePaths: RIO[FileIO, Vector[ResourceIndicator]]) extends TestCaseRunnerCompilePhase {

  override val name: String = "JavaScript Execution"
  override protected val backend: JSBackend.type = JSBackend

  override protected def backendOptions(compilerOptions: CompilerOptions[Id]): IO[IOException, JSBackendOptions[Id, ResourceIndicator]] =
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

  override protected def getProgramOutput(compOutput: CompilationOutputText): ZIO[BuildEnvironment, NonEmptyList[CompilationError], Either[Throwable, String]] =
    (
      for {
        references <- EitherT[ZIO[BuildEnvironment, NonEmptyList[CompilationError], *], Throwable, Vector[ResourceIndicator]](referencePaths.either)
        (compiledFile, _) <- EitherT.liftF(compOutput.textStream.foldLeftM("") { (a, b) => IO.succeed(a + b) } : ZIO[BuildEnvironment, ErrorList, (String, Unit)])
        output <- EitherT[ZIO[BuildEnvironment, NonEmptyList[CompilationError], *], Throwable, String](runJSOutput(references)(compiledFile).either)
      } yield output
    ).value


  override def runTest(testCase: TestCase): ZIO[BuildEnvironment, Throwable, TestCaseResult] =
    referencePaths.flatMap { references =>
      compileTestCase(testCase, references)
    }

  private def runJSOutput(files: Vector[ResourceIndicator])(compiledFile: String): ZIO[BuildEnvironment, Throwable, String] = for {
    referenceLibs <- files.traverse { path =>
      for {
        (libName, parentDir) <- path match {
          case PathResourceIndicator(path) => IO.succeed((path.fileNameWithoutExtension, path.parent))
          case _ => IO.fail(new IOException("Unexpected resource indicator"))
        }
        currentDir <- Path.of(".")
        libFilePath <- Path.of("js", libName + ".js")
        libFile = parentDir.getOrElse(currentDir).resolve(libFilePath)
        content <- ZIO.accessM[FileIO] { _.get.readAllText(libFile) }
      } yield FileInfo(libName, content)
    }

    modules = (referenceLibs :+ FileInfo(moduleName, compiledFile))

    output <- executeJS(compiledFile)(modules)
  } yield output

  protected def executeJS(compiledFile: String)(modules: Seq[FileInfo]): ZIO[BuildEnvironment, Throwable, String]

}

