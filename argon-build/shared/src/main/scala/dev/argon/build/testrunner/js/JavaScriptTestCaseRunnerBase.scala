package dev.argon.build.testrunner.js

import java.io.{FileNotFoundException, IOException}

import dev.argon.io.Path
import dev.argon.io.Path.PathExtensions
import dev.argon.build.testrunner._
import cats._
import cats.data.{EitherT, NonEmptyList, NonEmptyVector}
import cats.implicits._
import dev.argon.backend.{Backend, ResourceWriter}
import zio._
import zio.interop.catz.core._
import dev.argon.compiler.{CompilationError, ErrorList}
import dev.argon.backend.js.{JSBackend, JSBackendOptions, JSInjectCode}
import dev.argon.io.fileio.FileIO
import dev.argon.build._
import dev.argon.build.testrunner.TestCaseRunnerCompilePhase.TestCompileResource
import dev.argon.compiler.loaders.{ResourceIndicator, ResourceReader}
import dev.argon.compiler.options.{CompilerOptions, FileList}

abstract class JavaScriptTestCaseRunnerBase[I <: ResourceIndicator: Tag, P: Path : Tag](pathResolver: I => UIO[P]) extends TestCaseRunnerExecutionPhase[I, ResourceReader[I] with ResourceWriter[Nothing] with FileIO[P]] {

  override val name: String = "JavaScript Execution"
  override protected val backend: JSBackend

  override protected def backendOptions: UIO[JSBackendOptions[Id, TestCompileResource[I]]] =
    IO.succeed(
      JSBackendOptions[Id, TestCompileResource[I]](
        extern = new FileList(List.empty),
        inject = JSInjectCode[Id, TestCompileResource[I]](
          before = None,
          after = None,
        )
      )
    )

  override protected def getProgramOutput(compOutput: backend.TCompilationOutput): ZIO[FileIO[P], TestCaseError, String] =
    for {
      compiledFile <- compOutput.textStream.foldM("") { (a, b) => IO.succeed(a + b) }.mapError(compilationFailureResult)
      output <- runJSOutput(references)(compiledFile).mapError(executionFailureResult)
    } yield output

  private def runJSOutput(files: Vector[I])(compiledFile: String): RIO[FileIO[P], String] = for {
    referenceLibs <- files.traverse { id =>
      for {
        libPath <- pathResolver(id)
        libDir <- IO.fromEither(libPath.parent.toRight { new FileNotFoundException() })
        libName = libPath.fileNameWithoutExtension
        relPart <- Path.of("js", libName + ".js")
        content <- ZIO.accessM[FileIO[P]] { _.get.readAllText(libDir.resolve(relPart)) }
      } yield FileInfo(libName, content)
    }

    modules = (referenceLibs :+ FileInfo(moduleName, compiledFile))

    output <- executeJS(compiledFile)(modules)
  } yield output

  protected def executeJS(compiledFile: String)(modules: Seq[FileInfo]): Task[String]

}

