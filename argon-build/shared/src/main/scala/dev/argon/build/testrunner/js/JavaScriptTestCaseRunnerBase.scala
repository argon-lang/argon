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
import dev.argon.io.{FileIO, FilenameManip}
import dev.argon.stream.{Resource, Step, StreamTransformation}
import dev.argon.stream.{Step, StreamTransformation}
import dev.argon.build._

abstract class JavaScriptTestCaseRunnerBase(referencePaths: UIO[Vector[Path]]) extends TestCaseRunnerCompilePhase {

  override val name: String = "JavaScript Execution"
  override protected val backend: JSBackend.type = JSBackend

  override protected def backendOptions(compilerOptions: CompilerOptions[Id]): IO[IOException, JSBackendOptions[Id, Path]] =
    Path.of(compilerOptions.moduleName + ".js").map { outputPath =>
      JSBackendOptions[Id, Path](
        outputFile = outputPath,
        extern = Map.empty,
        inject = JSInjectCode[Id](
          before = None,
          after = None,
        )
      )
    }

  override protected def getProgramOutput(compOutput: CompilationOutputText { val context: Backend.ContextWithComp[ZIO, BuildEnvironment, Path] }): ZIO[BuildEnvironment, NonEmptyList[CompilationError], Either[Throwable, String]] = for {
    references <- referencePaths
    (compiledFile, _) <- compOutput.textStream.foldLeftM("") { (a, b) => IO.succeed(a + b) }
    output <- runJSOutput(references)(compiledFile).either
  } yield output


  override def runTest(testCase: TestCase): ZIO[BuildEnvironment, Throwable, TestCaseResult] =
    referencePaths.flatMap { references =>
      compileTestCase(testCase, references)
    }

  private def runJSOutput(files: Vector[Path])(compiledFile: String): ZIO[BuildEnvironment, Throwable, String] = for {
    referenceLibs <- files.traverse { path =>
      val libName = path.fileNameWithoutExtension

      for {
        currentDir <- Path.of(".")
        libFilePath <- Path.of("js", libName + ".js")
        libFile = path.parent.getOrElse(currentDir).resolve(libFilePath)
        content <- ZIO.accessM[FileIO] { _.fileIO.readAllText(libFile) }
      } yield FileInfo(libName, content)
    }

    modules = (referenceLibs :+ FileInfo(moduleName, compiledFile))

    output <- executeJS(compiledFile)(modules)
  } yield output

  protected def executeJS(compiledFile: String)(modules: Seq[FileInfo]): ZIO[BuildEnvironment, Throwable, String]

}

