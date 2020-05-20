package dev.argon.build.testrunner

import java.io.IOException

import dev.argon.io.{Path, ZipFileReader}
import dev.argon.build.BuildProcess
import dev.argon.compiler._
import dev.argon.compiler.core.ModuleDescriptor
import cats._
import cats.data.NonEmptyList
import zio._
import zio.stream._
import dev.argon.backend.{Backend, ResourceWriter}
import dev.argon.io.fileio.FileIO
import dev.argon.build._
import dev.argon.build.testrunner.TestCaseRunnerCompilePhase.{TestCaseInputSource, TestCaseOtherRes, TestCompileResource}
import dev.argon.compiler.loaders.{ResourceIndicator, ResourceReader}
import dev.argon.compiler.options.{CompilerOptions, FileList}
import scalapb.{GeneratedMessage, GeneratedMessageCompanion}

private[testrunner] abstract class TestCaseRunnerCompilePhase[I <: ResourceIndicator: Tag, -R <: ResourceReader[I]] extends TestCaseRunner[R] {

  protected val moduleName = "TestProgram"
  private val compilerOptions = CompilerOptions[Id, TestCompileResource[I]](
    moduleName = moduleName,
    inputFiles = new FileList(List.empty),
    references = new FileList(List.empty),
  )

  protected val backend: Backend

  protected def backendOptions: Task[backend.BackendOptions[Id, TestCompileResource[I]]]

  protected final def compileTestCase(testCase: TestCase, references: Vector[I]): ZManaged[R, TestCaseError, backend.TCompilationOutput] =
    ZManaged.fromEffect(backendOptions)
      .mapError(TestCaseActualResult.ExecutionError.apply)
      .flatMap { backendOpts =>
        BuildProcess.compile[TestCompileResource[I]](
          backend : backend.type
        )(
          compilerOptions.copy[Id, TestCompileResource[I]](
            inputFiles = new FileList[TestCompileResource[I]](testCase.sourceCode.map(TestCaseInputSource[I]).toList),
            references = new FileList[TestCompileResource[I]](references.map(TestCaseOtherRes.apply).toList),
          ) : CompilerOptions[Id, TestCompileResource[I]],
          backendOpts : backend.BackendOptions[Id, TestCompileResource[I]],
        )
          .mapError(compilationFailureResult)
          .provideSomeLayer[R](TestCaseRunnerCompilePhase.testCompileResourceReaderLayer[I])
      }


}

object TestCaseRunnerCompilePhase {

  sealed trait TestCompileResource[I <: ResourceIndicator] extends ResourceIndicator
  final case class TestCaseInputSource[I <: ResourceIndicator](inputSource: InputSourceData) extends TestCompileResource[I] {
    override def extension: String = {
      val slash = inputSource.name.lastIndexOf('/')
      val dot = inputSource.name.lastIndexOf('.')

      if(dot < 0 || dot < slash)
        ""
      else
        inputSource.name.substring(dot + 1)
    }

    override def show: String = inputSource.name
  }
  final case class TestCaseOtherRes[I <: ResourceIndicator](id: I) extends TestCompileResource[I] {
    override def extension: String = id.extension
    override def show: String = id.show
  }

  private def testCompileResourceReaderLayer[I <: ResourceIndicator : Tag]: URLayer[ResourceReader[I], ResourceReader[TestCompileResource[I]]] =
    ZLayer.fromFunction { env =>
      new ResourceReader.Service[TestCompileResource[I]] {
        override def readTextFile(id: TestCompileResource[I]): Stream[ErrorList, Char] =
          id match {
            case TestCaseInputSource(inputSource) => Stream.fromChunk(Chunk.fromArray(inputSource.data.toCharArray))
            case TestCaseOtherRes(id) => env.get.readTextFile(id)
          }

        override def readTextFileAsString(id: TestCompileResource[I]): Comp[String] =
          id match {
            case TestCaseInputSource(inputSource) => IO.succeed(inputSource.data)
            case TestCaseOtherRes(id) => env.get.readTextFileAsString(id)
          }

        override def getZipReader(id: TestCompileResource[I]): Managed[ErrorList, ZipFileReader[Any, ErrorList]] =
          id match {
            case TestCaseInputSource(_) =>
              Managed.fail(NonEmptyList.of(CompilationError.ResourceIOError(
                CompilationMessageSource.ThrownException(new IOException("Invalid file format. Not a zip file."))
              )))
            case TestCaseOtherRes(id) => env.get.getZipReader(id)
          }

        override def deserializeProtocolBuffer[L[_, _], A <: GeneratedMessage](companion: GeneratedMessageCompanion[A])(data: stream.Stream[ErrorList, Byte]): Comp[A] =
          env.get.deserializeProtocolBuffer(companion)(data)
      }
    }

}

