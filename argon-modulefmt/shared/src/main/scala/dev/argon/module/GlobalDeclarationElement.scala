package dev.argon.module

import com.google.protobuf.{CodedInputStream, CodedOutputStream}
import scalapb.LiteParser
import zio.stream.ZStream

sealed trait GlobalDeclarationElement
object GlobalDeclarationElement {
  final case class TraitElement(traitElement: GlobalDeclarationType) extends GlobalDeclarationElement
  final case class ClassElement(classElement: GlobalDeclarationType) extends GlobalDeclarationElement
  final case class DataConstructorElement(dataConstructorElement: GlobalDeclarationType) extends GlobalDeclarationElement
  final case class FunctionElement(functionElement: GlobalDeclarationFunction) extends GlobalDeclarationElement
}



