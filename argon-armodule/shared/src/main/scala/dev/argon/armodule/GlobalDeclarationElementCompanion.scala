package dev.argon.armodule

import com.google.protobuf.{CodedInputStream, CodedOutputStream}
import dev.argon.module.{GlobalDeclarationElement, GlobalDeclarationFunction, GlobalDeclarationList, GlobalDeclarationType}
import dev.argon.module.GlobalDeclarationElement._
import dev.argon.util.StreamableMessage
import scalapb.LiteParser

object GlobalDeclarationElementCompanion extends StreamableMessage[GlobalDeclarationElement] {
  override def writeElement(value: GlobalDeclarationElement)(output: CodedOutputStream): Unit = {
    val list = value match {
      case TraitElement(traitElement) => GlobalDeclarationList(globalTraits = Vector(traitElement))
      case ClassElement(classElement) => GlobalDeclarationList(globalClasses = Vector(classElement))
      case DataConstructorElement(dataConstructorElement) => GlobalDeclarationList(globalDataConstructors = Vector(dataConstructorElement))
      case FunctionElement(functionElement) => GlobalDeclarationList(globalFunctions = Vector(functionElement))
    }
    list.writeTo(output)
  }

  @SuppressWarnings(Array("org.wartremover.warts.Null"))
  override def readElement(input: CodedInputStream): GlobalDeclarationElement =
   input.readTag() match {
      case 0 => null
      case 10 => TraitElement(LiteParser.readMessage(input, GlobalDeclarationType.defaultInstance))
      case 18 => ClassElement(LiteParser.readMessage(input, GlobalDeclarationType.defaultInstance))
      case 26 => DataConstructorElement(LiteParser.readMessage(input, GlobalDeclarationType.defaultInstance))
      case 34 => FunctionElement(LiteParser.readMessage(input, GlobalDeclarationFunction.defaultInstance))
      case tag =>
        val _ = input.skipField(tag)
        readElement(input)
    }

}
