package dev.argon.build.testrunner

import dev.argon.build.BuildProcess
import dev.argon.compiler._
import dev.argon.compiler.core.{Context, ModuleId}
import cats._
import zio._
import zio.stream._
import dev.argon.backend.Backend
import dev.argon.io.fileio.{FileIO, ZipRead}
import dev.argon.build._
import dev.argon.compiler.options.{CompilerInput, CompilerOptions}
import dev.argon.io.ZipCreator
import dev.argon.options.{FileList, Options}
import dev.argon.parser.impl.ArgonSourceParser
import dev.argon.util.{FileID, FileSpec, MaybeBlocking, ProtoBufCodecs, StreamableMessage}

private[testrunner] abstract class TestCaseRunnerCompilePhase[-R <: FileIO with ZipRead with MaybeBlocking] extends TestCaseRunner[R] {

  protected val moduleName = "TestProgram"

  protected val backend: Backend

  protected def backendOptions: Task[Options[Id, backend.BackendOptionID]] =
    backend.backendOptions.inferDefaults(backend.backendOptions.empty[Id]) match {
      case Left(id) =>
        val optionName = backend.backendOptions.info.get(id).name
        IO.fail(new RuntimeException("Missing field in backend options: " + optionName))
      case Right(value) => IO.succeed(value)
    }

  protected final def compileTestCase(testCase: TestCase, references: FileList): ZManaged[R, CompilationError, BuildResult.Aux[backend.type]] = {
    val compilerOptions = CompilerOptions[Id](
      moduleName = moduleName,
      inputFiles = new FileList(List.empty),
      references = references,
    )

    val sourceCodeStream = ZStream.fromIterable(testCase.sourceCode.zipWithIndex)
      .flatMap { case (inputSourceData, index) =>
        val inputFile = FileSpec(FileID(index), inputSourceData.name)

        val fileStream = ZStream.fromChunk(Chunk.fromArray(inputSourceData.data.toCharArray))

        ArgonSourceParser.parse(inputFile)(fileStream)
      }

    for {
      backendOpts <- ZManaged.fromEffect(backendOptions.orDie)
      compilerInput = CompilerInput(compilerOptions, backendOpts)
      buildResult <- BuildProcess.buildResult(backend)(compilerInput, sourceCodeStream)
    } yield buildResult
  }


}

