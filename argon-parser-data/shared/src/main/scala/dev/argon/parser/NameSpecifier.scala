package dev.argon.parser

import dev.argon.parser.Token.OperatorToken

sealed trait NameSpecifier derives CanEqual

object NameSpecifier {

  case object Blank extends NameSpecifier
  final case class Identifier(name: String) extends NameSpecifier
  final case class Operator(op: OperatorToken) extends NameSpecifier

}
