package dev.argon.build.testrunner.js

import java.io.{FileNotFoundException, IOException}

import dev.argon.io.Path
import dev.argon.io.Path.PathExtensions
import dev.argon.build.testrunner._
import cats._
import cats.data.{EitherT, NonEmptyList, NonEmptyVector}
import cats.implicits._
import dev.argon.backend.{ResourceReader, ResourceWriter}
import zio._
import zio.interop.catz.core._
import dev.argon.compiler.{CompilationError, CompilerOptions, ErrorList}
import dev.argon.backend.js.{JSBackend, JSBackendOptions, JSInjectCode}
import dev.argon.io.fileio.FileIO
import dev.argon.build._
import dev.argon.compiler.loaders.ResourceIndicator
import dev.argon.module.PathResourceIndicator

abstract class JavaScriptTestCaseRunnerBase[I <: ResourceIndicator: Tagged, P: Path : Tagged](pathResolver: I => UIO[P]) extends TestCaseRunnerExecutionPhase[I, ResourceReader[I] with ResourceWriter[Nothing] with FileIO[P]] {

  override val name: String = "JavaScript Execution"
  override protected val backend: JSBackend.type = JSBackend

  override protected def backendOptions(compilerOptions: CompilerOptions[Id]): UIO[JSBackendOptions[Id, I]] =
    IO.succeed(
      JSBackendOptions[Id, I](
        extern = Map.empty,
        inject = JSInjectCode[Id](
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

