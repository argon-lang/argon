package dev.argon.compiler.types

import dev.argon.parser
import dev.argon.parser.FunctionParameterListType

sealed trait ParameterStyle
object ParameterStyle {
  object Normal extends ParameterStyle
  object Inferrable extends ParameterStyle

  def fromParser(listType: parser.FunctionParameterListType): ParameterStyle =
    listType match {
      case FunctionParameterListType.NormalList => Normal
      case FunctionParameterListType.InferrableList => Inferrable
    }
}
