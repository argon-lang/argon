package com.mi3software.argon.build.testrunner.node

import java.io.{File, FileInputStream, PrintWriter, StringWriter}
import java.nio.charset.StandardCharsets

import com.mi3software.argon.build.JSBackend
import com.mi3software.argon.build.testrunner._
import scalaz._
import Scalaz._
import scalaz.zio._
import scalaz.zio.interop.scalaz72._
import com.mi3software.argon.build.testrunner.node.ExternalApi._
import com.mi3software.argon.util.FileOperations
import org.apache.commons.io.{FilenameUtils, IOUtils}

import scala.concurrent.Await
import scala.concurrent.duration.Duration

final class NodeTestCaseRunner(references: Vector[File], launcher: NodeLauncher) extends TestCaseRunnerCompilePhase {
  override def runTest(testCase: TestCase): IO[Throwable, TestCaseResult] =
    compileTestCase(testCase, JSBackend, references)
      .flatMap { case (compileOutput, expectedOutput) =>

        EitherT(
          IO.syncThrowable {
            val writer = new StringWriter()
            val printWriter = new PrintWriter(writer)
            compileOutput.writeText(printWriter)
            printWriter.close()
            writer.toString
          }
            .flatMap(runJSOutput(references))
            .map { output =>
              if(output.trim === expectedOutput.trim)
                TestCaseResult.Success
              else
                TestCaseResult.Failure(
                  TestCaseActualResult.Output(output),
                  TestCaseExpectedOutput(expectedOutput)
                )
            }
            .map(\/.right[TestCaseResult, TestCaseResult])
        )
      }
      .run
      .map { _.merge}

  private def runJSOutput(files: Vector[File])(compiledFile: String): IO[Throwable, String] = IO.syncThrowable {

    val modules = (
      files.map { file =>
        val libName = FilenameUtils.removeExtension(file.getName)
        val libFile = new File(new File(file.getParentFile, "js"), libName + ".js")
        val content = IOUtils.toString(new FileInputStream(libFile), StandardCharsets.UTF_8)
        FileInfo(libName, content)
      }
      :+ FileInfo(moduleDescriptor.name, compiledFile)
    ).toArray

    Await.result(launcher.serverFunctions.executeJS(moduleDescriptor.name, modules), Duration.Inf)
  }
}
