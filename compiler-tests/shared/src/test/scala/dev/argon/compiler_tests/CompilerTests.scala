package dev.argon.compiler_tests

import zio.*
import zio.stream.*
import zio.test.{TestExecutor as _, *}
import zio.test.Assertion.*
import dev.argon.compiler.ErrorLog
import dev.argon.util.xml.XmlParser
import java.io.IOException
import fs2.data.xml.XmlException
import esexpr.{ESExpr, ESExprCodec}
import dev.argon.util.xml.XmlDocumentCountException
import dev.argon.build.*
import dev.argon.compiler_tests.TestCase.ExpectedResult
import dev.argon.compiler.*
import cats.data.NonEmptySeq
import dev.argon.util.async.ZIOErrorUtil
import esexpr.parser.ESExprTextReader

import dev.argon.util.{*, given}
import dev.argon.io.PathUtil
import java.io.StringWriter
import java.io.PrintWriter

object CompilerTests extends ZIOSpecDefault {

  type Env = Any
  type Error = IOException | BuildError

  override def spec: Spec[TestEnvironment & Scope, Any] =
    suite("Compiler Tests")(
      buildPluginTests
    )

  private def buildPluginTests: Spec[Env, Error] =
    ???

  
  private def buildTestCases(context: Context): Spec[Env, Error] =

    def runTestCompile(testCase: TestCase): ZIO[Env, Error, TestResult] =
      ???

    def createTest(testCase: TestCase): Spec[Env, Error] =
      test(testCase.name) {
        for
          res <- runTestCompile(testCase)
        yield testCase.expectedResult match {
          case ExpectedResult.ExpectedOutput(text) =>
            assert(res)(outputMatches(text))

          case _ => ???
        }
      }


    def groupTestCases(testCases: Map[Seq[String], TestCase]): Seq[Spec[Env, Error]] =
        testCases
          .groupBy {
            case (h +: _ +: _, _) => Some(h)
            case _ => None
          }
          .toSeq
          .flatMap {
            case (Some(k), subdir) =>
              Seq(suite(k)(groupTestCases(subdir.map { (k, v) => k.drop(1) -> v })*))

            case (None, testCases) =>

              testCases.values
                .map(createTest)
          }

    Spec.scoped(
      ZIO.foreach(testCases) { (path, testCase) =>
        for
          doc <- XmlParser.parse(testCase).orDie
          tc = TestCase.fromXml(doc.docElem)
        yield path.split("/").nn.toSeq.map(_.nn) -> tc
      }.map(tcm => Spec.multiple(Chunk.fromIterable(groupTestCases(tcm))))
    )
  end buildTestCases


  private def outputMatches(expected: String): Assertion[TestResult] =
    Assertion.assertion("Output matches") { actual =>
      def normalize(s: String): String =
        s.trim().nn.split("\\n").nn.map(_.nn.trim().nn).mkString("\n")

      actual match {
        case TestResult.Success(actual) =>
          normalize(expected) == normalize(actual)

        case _ => false
      }
    }

  private enum TestResult {
    case Success(output: String)
    case ExecutionError(message: String)
    case CompileError(errors: Seq[dev.argon.compiler.CompilerError])
  }

      

  private final case class FullProgram[CompiledProgram](libraries: Map[TubeName, CompiledProgram], program: CompiledProgram)

  private def testCaseToSourcesMap(testCase: TestCase): Map[String, String] =
    testCase.sources.map { is =>
      ("Test/src/" + is.name) -> is.content
    }.toMap

}
