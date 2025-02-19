package dev.argon.compiler_tests

import dev.argon.io.*
import dev.argon.io.xml.*
import zio.*
import zio.stream.*
import java.nio.charset.CharacterCodingException
import fs2.data.xml.XmlException
import scala.xml.Document

trait TestCaseResource[+E] extends XmlResource[E] {
  def asTestCase: IO[E, TestCase]
}

object TestCaseResource {
  given [E >: CharacterCodingException | XmlException | XmlDocumentCountException] => BinaryResourceDecoder[TestCaseResource, E]:
    override def decode(resource: BinaryResource[E]): TestCaseResource[E] =
      resource match {
        case resource: TestCaseResource[E] => resource
        case _ =>
          new TestCaseResource[E] {
            override def asTestCase: IO[E, TestCase] =
              asXmlDocument.map(doc => TestCase.fromXml(doc.docElem))

            override def asXmlDocument: IO[E, Document] =
              resource.decode[XmlResource].asXmlDocument

            override def asText: Stream[E, String] =
              resource.decode[TextResource].asText

            override def asBytes: Stream[E, Byte] =
              resource.asBytes

            override def fileName: Option[String] =
              resource.fileName
          }
      }
  end given
}
