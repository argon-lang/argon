package dev.argon.io.xml

import dev.argon.io.*
import scala.xml.Document
import zio.*
import zio.stream.*
import zio.stream.interop.fs2z.*
import zio.interop.catz.given
import java.nio.charset.CharacterCodingException
import fs2.data.xml.scalaXml.given

import dev.argon.util.async.ErrorWrapper

trait XmlResource[+E] extends TextResource[E] {
  def asXmlDocument: IO[E, Document]
}

object XmlResource {
  trait Impl[+E >: CharacterCodingException] extends XmlResource[E] with TextResource.Impl[E] {
    override def asText: Stream[E, String] =
      val errorContext = ErrorWrapper.Context[E]
      import errorContext.given
      
      ErrorWrapper.unwrapStream(
        ZStream.fromZIO(ErrorWrapper.wrapEffect(asXmlDocument))
          .toFs2Stream
          .through(fs2.data.xml.dom.eventify)
          .through(fs2.data.xml.render.raw())
          .toZStream()
      )
        
    end asText
  }

  given [E >: CharacterCodingException]: BinaryResourceDecoder[XmlResource, E] with
    override def decode(resource: BinaryResource[E]): XmlResource[E] =
      resource match {
        case resource: XmlResource[E] => resource
        case _ =>
          new XmlResource[E] {
            override def asXmlDocument: IO[E, Document] =
              val errorContext = ErrorWrapper.Context[E]
              import errorContext.given

              ErrorWrapper.unwrapEffect(
                ErrorWrapper.wrapStream(asText)
                  .toFs2Stream
                  .through(fs2.data.xml.events[Task, String]())
                  .through(fs2.data.xml.dom.documents)
                  .toZStream()
                  .take(2)
                  .runCollect
                  .flatMap {
                    case Seq(doc) => ZIO.succeed(doc)
                    case _ => ZIO.fail(XmlDocumentCountException())
                  }
              )
            end asXmlDocument

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
