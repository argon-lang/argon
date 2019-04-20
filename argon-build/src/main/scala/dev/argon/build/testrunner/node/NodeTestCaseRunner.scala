package dev.argon.build.testrunner.node

import java.io.{File, FileInputStream, PrintWriter, StringWriter}
import java.nio.charset.StandardCharsets

import dev.argon.build.{Backend, CompilationOutputText, JSBackend}
import dev.argon.build.testrunner._
import scalaz.{Scalaz, _}
import Scalaz._
import scalaz.zio._
import dev.argon.build.testrunner.node.ExternalApi._
import dev.argon.compiler.CompilerOptions
import dev.argon.compiler.js.{JSBackendOptions, JSInjectCode}
import dev.argon.util.{FileOperations, FilenameManip}
import org.apache.commons.io.IOUtils

final class NodeTestCaseRunner(references: Vector[File], launcher: NodeLauncher) extends TestCaseRunnerCompilePhase {

  override protected val backend: JSBackend.type = JSBackend

  override protected def backendOptions(compilerOptions: CompilerOptions[Id]): IO[Throwable, JSBackendOptions[Id, File]] = for {
    outFile <- FileOperations.fileFromName(compilerOptions.moduleName + ".js")
  } yield JSBackendOptions[Id, File](
    outputFile = outFile,
    extern = Map.empty,
    inject = JSInjectCode[Id](
      before = None,
      after = None,
    )
  )

  override protected def getProgramOutput(compOutput: CompilationOutputText[IO[Throwable, +?], File]): IO[Throwable, String] =
    IO.effect {
      val writer = new StringWriter()
      val printWriter = new PrintWriter(writer)
      compOutput.writeText(printWriter)
      printWriter.close()
      writer.toString
    }
      .flatMap(runJSOutput(references))

  override def runTest(testCase: TestCase): IO[Throwable, TestCaseResult] =
    compileTestCase(testCase, references)

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

}
