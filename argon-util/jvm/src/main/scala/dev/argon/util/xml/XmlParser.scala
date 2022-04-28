package dev.argon.util.xml

import scala.collection.mutable
import java.io.{Reader, StringReader}
import zio.*
import zio.stream.*

import javax.xml.namespace.QName
import javax.xml.stream.{XMLInputFactory, XMLStreamConstants, XMLStreamException, XMLStreamReader}
import scala.util.Using
import scala.util.Using.Releasable
import scala.reflect.TypeTest

object XmlParser {

  def parse(text: String): IO[XMLException, Element] =
    parse(new StringReader(text)).refineToOrDie[XMLException]

  def parse[R, E >: XMLException <: Throwable](stream: ZStream[R, E, Char])(using TypeTest[Throwable, E]): ZIO[R, E, Element] =
    ZIO.scoped(
      stream.toReader
        .flatMap(parse)
        .refineOrDie { case ex: E => ex }
    )

  private given Releasable[XMLStreamReader] with
    def release(reader: XMLStreamReader): Unit =
      reader.close()
  end given

  def parse(reader: Reader): Task[Element] =
    IO.attemptBlockingInterrupt {
      val xmlif = XMLInputFactory.newFactory()
      Using.resource(xmlif.createXMLStreamReader(reader))(parseImpl)
    }

  private def parseImpl(reader: XMLStreamReader): Element =
    import XMLStreamConstants.*
    val elements = mutable.Stack.empty[(Element, mutable.ArrayBuffer[Node])]

    while true do
      reader.next() match {
        case START_ELEMENT =>
          def convertName(name: QName): Name =
            Name(Namespace(name.getNamespaceURI), name.getLocalPart)


          val elemName = convertName(reader.getName)
          val attributes = new mutable.ArrayBuffer[Attribute](reader.getAttributeCount)
          for i <- 0 until reader.getAttributeCount do
            val attrName = convertName(reader.getAttributeName(i))
            attributes.addOne(Attribute(attrName, reader.getAttributeValue(i)))
          end for

          val element = Element(elemName, attributes.toSeq, Seq.empty)
          elements.push((element, mutable.ArrayBuffer.empty))

        case END_ELEMENT =>
          val (element, children) = elements.pop()
          val element2 = element.copy(children = children.toSeq)
          if elements.isEmpty then
            return element2
          else
            elements.top._2.addOne(element2)

        case CHARACTERS | SPACE =>
          elements.top._2.addOne(Characters(reader.getText))

        case _ => ()
      }
    end while

    if elements.isEmpty then
      throw new XMLStreamException("Could not find root element")
    else
      throw new XMLStreamException("Could not find closing tag of root element")

  end parseImpl


}
