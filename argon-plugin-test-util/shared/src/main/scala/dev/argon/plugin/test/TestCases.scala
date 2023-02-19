package dev.argon.plugin.test

import dev.argon.util.test.{CompileTimeFileSystem, ReadFileCompileTime}
import dev.argon.util.{*, given}
import scala.xml
import dev.argon.util.xml.XmlParser

import zio.*

private[test] enum TestCases {
  case Group(tests: Map[String, TestCases])
  case Test(test: TestCase)
}

private[test] object TestCases {
  private val testCasesRaw: CompileTimeFileSystem = ReadFileCompileTime.readDirectory("testcases", (isDir, name) => isDir || name.endsWith(".xml"))

  private def convertTestCases(fs: CompileTimeFileSystem): Task[TestCases] =
    fs match
      case CompileTimeFileSystem.Directory(entries) =>
        ZIO.foreach(entries) { case (name, fs) =>
          for
            testCases <- convertTestCases(fs)
          yield (name, testCases)
        }.map(TestCases.Group.apply)

      case CompileTimeFileSystem.File(data) =>
        parseTestCase(data).map(TestCases.Test.apply)
    end match

  private def parseTestCase(data: String): Task[TestCase] =
    XmlParser.parse(data).map { doc =>
      TestCase(
        name = (doc \ "Name").collectFirst { case elem: xml.Elem => elem }.get.text,
        inputSources = (doc \ "InputSource")
          .collect {
            case inputSourceNode: xml.Elem =>
              ((inputSourceNode \ "@name").text, inputSourceNode.text)
          }
          .toMap,
        expectedResult =
          (doc \ "ExpectedOutput")
            .collectFirst {
              case node: xml.Elem => TestCase.ExpectedResult.Output(node.text)
            }
            .orElse {
              (doc \ "ExpectedError")
                .collectFirst {
                  case node: xml.Elem => TestCase.ExpectedResult.Error(node.text)
                }
            }
            .get,
      )
    }

  def testCases: Task[TestCases] =
    convertTestCases(testCasesRaw)

}
