package com.mi3software.argon.parser.impl

import com.mi3software.argon.parser.BinaryOperator

import scalaz.NonEmptyList

sealed trait Token
object Token {

  sealed trait BinaryOperatorToken extends Token{
    def binaryOperator: BinaryOperator
  }

  final case class StringToken(parts: NonEmptyList[StringToken.Part]) extends Token
  object StringToken {
    sealed trait Part
    final case class StringPart(str: String) extends Part
  }

  final case class IntToken(value: BigInt) extends Token

  final case class Identifier(name: String) extends Token

  case object NewLine extends Token
  case object Semicolon extends Token

  case object KW_DEF extends Token
  case object KW_PROC extends Token
  case object KW_INSTANCE extends Token
  case object KW_CONSTRUCTOR extends Token
  case object KW_END extends Token
  case object KW_DO extends Token
  case object KW_VAR extends Token
  case object KW_VAL extends Token
  case object KW_TRUE extends Token
  case object KW_FALSE extends Token
  case object KW_AS extends Token
  case object KW_NAMESPACE extends Token
  case object KW_IMPORT extends Token
  case object KW_TRAIT extends Token
  case object KW_STATIC extends Token
  case object KW_DATA extends Token
  case object KW_PUBLIC extends Token
  case object KW_PROTECTED extends Token
  case object KW_PRIVATE extends Token
  case object KW_INTERNAL extends Token
  case object KW_BASE extends Token
  case object KW_IF extends Token
  case object KW_THEN extends Token
  case object KW_ELSE extends Token
  case object KW_ELSIF extends Token
  case object KW_OPEN extends Token
  case object KW_SEALED extends Token
  case object KW_VIRTUAL extends Token
  case object KW_ABSTRACT extends Token
  case object KW_OVERRIDE extends Token
  case object KW_FINAL extends Token
  case object KW_TYPE extends Token
  case object KW_MATCH extends Token
  case object KW_CASE extends Token
  case object KW_CLASS extends Token
  case object KW_NEW extends Token
  case object KW_FIELD extends Token
  case object KW_INITIALIZE extends Token
  case object KW_UNDERSCORE extends Token
  case object KW_GC extends Token
  case object KW_STRUCT extends Token
  case object KW_STACK extends Token
  case object KW_ANY extends Token
  case object KW_VALUETYPE extends Token

  case object OP_BOOLAND extends Token
  case object OP_BOOLOR extends Token
  case object OP_EQUALS extends BinaryOperatorToken {
    override def binaryOperator: BinaryOperator = BinaryOperator.Equal
  }
  case object OP_NOTEQUALS extends BinaryOperatorToken {
    override def binaryOperator: BinaryOperator = BinaryOperator.NotEqual
  }
  case object OP_LESSTHANEQ extends BinaryOperatorToken {
    override def binaryOperator: BinaryOperator = BinaryOperator.LessThanEq
  }
  case object OP_GREATERTHANEQ extends BinaryOperatorToken {
    override def binaryOperator: BinaryOperator = BinaryOperator.GreaterThanEq
  }
  case object OP_SHIFTLEFT extends BinaryOperatorToken {
    override def binaryOperator: BinaryOperator = BinaryOperator.ShiftLeft
  }
  case object OP_SHIFTRIGHT extends BinaryOperatorToken {
    override def binaryOperator: BinaryOperator = BinaryOperator.ShiftRight
  }
  case object OP_ASSIGN extends Token
  case object OP_DOT extends Token
  case object OP_COMMA extends Token
  case object OP_OPENPAREN extends Token
  case object OP_CLOSEPAREN extends Token
  case object OP_OPENBRACKET extends Token
  case object OP_CLOSEBRACKET extends Token
  case object OP_OPENCURLY extends Token
  case object OP_CLOSECURLY extends Token
  case object OP_BOOLNOT extends Token
  case object OP_ADD extends BinaryOperatorToken {
    override def binaryOperator: BinaryOperator = BinaryOperator.Add
  }
  case object OP_SUB extends BinaryOperatorToken {
    override def binaryOperator: BinaryOperator = BinaryOperator.Sub
  }
  case object OP_MUL extends BinaryOperatorToken {
    override def binaryOperator: BinaryOperator = BinaryOperator.Mul
  }
  case object OP_DIV extends BinaryOperatorToken {
    override def binaryOperator: BinaryOperator = BinaryOperator.Div
  }
  case object OP_BITAND extends BinaryOperatorToken {
    override def binaryOperator: BinaryOperator = BinaryOperator.BitAnd
  }
  case object OP_BITOR extends BinaryOperatorToken {
    override def binaryOperator: BinaryOperator = BinaryOperator.BitOr
  }
  case object OP_BITXOR extends BinaryOperatorToken {
    override def binaryOperator: BinaryOperator = BinaryOperator.BitXOr
  }
  case object OP_BITNOT extends Token
  case object OP_LESSTHAN extends BinaryOperatorToken {
    override def binaryOperator: BinaryOperator = BinaryOperator.LessThan
  }
  case object OP_GREATERTHAN extends BinaryOperatorToken {
    override def binaryOperator: BinaryOperator = BinaryOperator.GreaterThan
  }
  case object OP_COLON extends Token
  case object OP_SUBTYPE extends Token
  case object OP_SUPERTYPE extends Token
  case object OP_LAMBDA_TYPE extends Token
  case object OP_LAMBDA extends Token
}
