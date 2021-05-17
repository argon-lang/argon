package dev.argon.armodule

import com.google.protobuf.{CodedInputStream, CodedOutputStream}
import dev.argon.module.{NamespaceDeclaration, NamespaceDeclarationList}
import dev.argon.util.StreamableMessage
import scalapb.LiteParser

object NamespaceDeclarationCompanion extends StreamableMessage[NamespaceDeclaration] {
  override def writeElement(value: NamespaceDeclaration)(output: CodedOutputStream): Unit =
    NamespaceDeclarationList(Vector(value)).writeTo(output)

  @SuppressWarnings(Array("scalafix:DisableSyntax.null"))
  override def readElement(input: CodedInputStream): NamespaceDeclaration =
    input.readTag() match {
      case 0 => null
      case 10 => LiteParser.readMessage(input, NamespaceDeclaration.defaultInstance)
      case tag =>
        val _ = input.skipField(tag)
        readElement(input)
    }
}
