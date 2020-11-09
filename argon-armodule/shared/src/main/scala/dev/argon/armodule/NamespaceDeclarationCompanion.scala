package dev.argon.armodule

import com.google.protobuf.{CodedInputStream, CodedOutputStream}
import dev.argon.io.StreamableMessage
import dev.argon.module.{NamespaceDeclaration, NamespaceDeclarationList}
import scalapb.LiteParser

object NamespaceDeclarationCompanion extends StreamableMessage[NamespaceDeclaration] {
  override def writeElement(value: NamespaceDeclaration)(output: CodedOutputStream): Unit =
    NamespaceDeclarationList(Vector(value)).writeTo(output)

  @SuppressWarnings(Array("org.wartremover.warts.Null"))
  override def readElement(input: CodedInputStream): NamespaceDeclaration =
    input.readTag() match {
      case 0 => null
      case 10 => LiteParser.readMessage(input, NamespaceDeclaration.defaultInstance)
      case tag =>
        val _ = input.skipField(tag)
        readElement(input)
    }
}
