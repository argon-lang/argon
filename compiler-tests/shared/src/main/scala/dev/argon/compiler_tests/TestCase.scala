package dev.argon.compiler_tests

import dev.argon.util.{*, given}
import dev.argon.compiler_tests.TestCase.{InputSource, ExpectedResult}
import scala.xml.Node
import dev.argon.io.*
import java.nio.charset.CharacterCodingException
import dev.argon.compiler.TubeName

final case class TestCase(
  name: String,
  sources: Seq[InputSource],
  expectedResult: ExpectedResult,
  libraries: Set[TubeName],
) {
  def toDirectoryResource: DirectoryResource[CharacterCodingException, TextResource] =
    MapDirectory(
      sources
        .view
        .map { is =>
          is.name -> is.content
        }
        .toMap
    )
}

object TestCase {
  final case class InputSource(name: String, content: String)

  object InputSource {
    def fromXml(elem: Node): InputSource =
      InputSource(
        name = elem.attribute("name").get.head.text,
        content = elem.text,
      )
  }

  enum ExpectedResult {
    case ExpectedOutput(text: String)
    case ExpectedError(error: String)
  }

  object ExpectedResult {
    def fromXml(parent: Node): ExpectedResult =
      (parent \ "ExpectedOutput").headOption match {
        case Some(outputNode) => ExpectedResult.ExpectedOutput(outputNode.text)
        case None => ExpectedResult.ExpectedError((parent \ "ExpectedError").head.text)
      }
  }


  def fromXml(elem: Node): TestCase =
    TestCase(
      name = (elem \ "Name").head.text,
      sources = (elem \ "InputSource").map(InputSource.fromXml),
      expectedResult = ExpectedResult.fromXml(elem),
      libraries = {
        val libs = (elem \ "Library").map(node => TubeName.decode(node.text).get).toSet
        if libs.isEmpty then
          Set(TubeName("Argon", "Core"))
        else
          libs
      },
    )
  

}

