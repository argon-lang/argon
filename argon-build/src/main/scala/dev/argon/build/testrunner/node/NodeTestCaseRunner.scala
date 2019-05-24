package dev.argon.build.testrunner.node

import java.io.{File, FileInputStream, IOException, PrintWriter, StringWriter}
import java.nio.charset.StandardCharsets

import dev.argon.build.{Backend, CompilationOutputText, JSBackend}
import dev.argon.build.testrunner._
import cats._
import cats.data.{NonEmptyList, NonEmptyVector}
import cats.instances._
import scalaz.zio._
import scalaz.zio.interop.catz._
import dev.argon.build.testrunner.node.ExternalApi._
import dev.argon.compiler.{CompilationError, CompilerOptions, IOCompilation}
import dev.argon.compiler.js.{JSBackendOptions, JSInjectCode}
import dev.argon.util.stream.{Step, StreamTransformation}
import dev.argon.util.{FileOperations, FilenameManip}
import org.apache.commons.io.IOUtils

final class NodeTestCaseRunner(references: Vector[File], launcher: NodeLauncher) extends TestCaseRunnerCompilePhase {

  override protected val backend: JSBackend.type = JSBackend

  override protected def backendOptions(compilerOptions: CompilerOptions[Id]): IO[IOException, JSBackendOptions[Id, File]] = for {
    outFile <- FileOperations.fileFromName(compilerOptions.moduleName + ".js")
  } yield JSBackendOptions[Id, File](
    outputFile = outFile,
    extern = Map.empty,
    inject = JSInjectCode[Id](
      before = None,
      after = None,
    )
  )

  override protected def getProgramOutput(compOutput: CompilationOutputText[ZIO, File]): IO[NonEmptyList[CompilationError], Either[Throwable, String]] = for {
    compiledFile <- compOutput.textStream.foldLeft(stringConcatTrans)
    output <- runJSOutput(references)(compiledFile).either
  } yield output


  override def runTest(rt: Runtime[_])(testCase: TestCase): IO[Throwable, TestCaseResult] =
    compileTestCase(rt)(testCase, references)

  private def runJSOutput(files: Vector[File])(compiledFile: String): IO[Throwable, String] = for {
    modules <- IO.effect {
      (
        files.map { file =>
          val libName = FilenameManip.getBasename(file)
          val libFile = new File(new File(file.getParentFile, "js"), libName + ".js")
          val content = IOUtils.toString(new FileInputStream(libFile), StandardCharsets.UTF_8)
          FileInfo(libName, content)
        }
        :+ FileInfo(moduleName, compiledFile)
      ).toArray
    }

    serverFuncs <- launcher.serverFunctions
    output <- IO.fromFuture { _ => serverFuncs.executeJS(moduleName, modules) }
  } yield output

  private def stringConcatTrans: StreamTransformation[ZIO, Any, NonEmptyList[CompilationError], String, Unit, Nothing, String] =
    new StreamTransformation[ZIO, Any, NonEmptyList[CompilationError], String, Unit, Nothing, String] {
      override type State = String

      override def initial: ZIO[Any, NonEmptyList[CompilationError], String] = IO.succeed("")

      override def step(s: String, ca: NonEmptyVector[String]): ZIO[Any, NonEmptyList[CompilationError], Step[String, String, Nothing, String]] =
        IO.succeed(Step.Continue(s + ca.toVector.mkString))

      override def end(s: String, result: Unit): ZIO[Any, NonEmptyList[CompilationError], (Vector[Nothing], ZIO[Any, NonEmptyList[CompilationError], String])] =
        IO.succeed((Vector(), IO.succeed(s)))
    }

}
