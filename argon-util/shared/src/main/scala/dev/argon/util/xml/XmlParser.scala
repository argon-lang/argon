package dev.argon.util.xml

import fs2.{*, given}
import fs2.data.xml.{*, given}
import fs2.data.xml.scalaXml.given
import fs2.data.xml.dom.documents
import zio.*
import zio.interop.catz.given

import scala.xml.Document

object XmlParser {
  def parse(input: String): IO[XmlException | XmlDocumentCountException, Document] =
    Stream.emit(input)
      .through(events[Task, String]())
      .through(documents[Task, Document])
      .compile
      .toList
      .flatMap {
        case List(doc) => ZIO.succeed(doc)
        case _ => throw XmlDocumentCountException()
      }
      .refineOrDie {
        case ex: XmlException => ex
        case ex: XmlDocumentCountException => ex
      }

}
