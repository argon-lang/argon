package com.mi3software.argon.build.testrunner.node

import java.io.{File, FileInputStream}

import com.mi3software.argon.build.JSBackend
import com.mi3software.argon.build.testrunner._
import scalaz._
import Scalaz._
import com.mi3software.argon.build.testrunner.node.ExternalApi._
import org.apache.commons.io.{FilenameUtils, IOUtils}
import scalaz.effect.IO

import scala.concurrent.Await
import scala.concurrent.duration.Duration

final class NodeTestCaseRunner(references: Vector[File], launcher: NodeLauncher) extends TestCaseRunnerCompilePhase {
  override def runTest(testCase: TestCase): IO[TestCaseResult] =
    compileTestCase(testCase, JSBackend, references)
      .flatMap { case (compileOutput, expectedOutput) =>

        EitherT(
          compileOutput.toByteArray
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

  private def runJSOutput(files: Vector[File])(compiledFile: Array[Byte]): IO[String] = IO {

    val modules = (
      files.map { file =>
        val libName = FilenameUtils.removeExtension(file.getName)
        val libFile = new File(new File(file.getParentFile, "js"), libName + ".js")
        val content = IOUtils.toString(new FileInputStream(libFile), "UTF-8")
        FileInfo(libName, content)
      }
      :+ FileInfo(moduleDescriptor.name, new String(compiledFile, "UTF-8"))
    ).toArray

    Await.result(launcher.serverFunctions.executeJS(moduleDescriptor.name, modules), Duration.Inf)
  }
}
