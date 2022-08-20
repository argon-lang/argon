package dev.argon.plugin.test

import dev.argon.util.{*, given}
import dev.argon.util.xml.*
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
    XmlParser.parse(data).map { xml =>
      TestCase(
        name = xml.child(Name("Name")).get.textContent,
        inputSources = xml.childrenByTag(Name("InputSource"))
          .map { inputSourceNode =>
            (inputSourceNode.attribute(Name("name")).get.value, inputSourceNode.textContent)
          }
          .toMap,
        expectedResult =
          xml.child(Name("ExpectedOutput")) match {
            case Some(node) => TestCase.ExpectedResult.Output(node.textContent)
            case None =>
              TestCase.ExpectedResult.Error(xml.child(Name("ExpectedError")).get.textContent)
          },
      )
    }

  def testCases: Task[TestCases] =
    convertTestCases(testCasesRaw)

}
