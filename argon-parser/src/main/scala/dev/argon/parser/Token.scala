package dev.argon.parser

import dev.argon.ast.*
import dev.argon.util.*
import cats.data.NonEmptyList

sealed trait Token(val category: TokenCategory) derives CanEqual

object Token {

  trait TokenFactory[TToken <: Token] {
    val category: TokenCategory
  }

  sealed trait BinaryOperatorToken(val binOperator: BinaryOperator) {
    self: Token =>
  }

  sealed trait UnaryOperatorToken(val unOperator: UnaryOperator) {
    self: Token =>
  }

  sealed trait ModifierToken(val modifier: Modifier) {
    self: Token =>
  }

  final case class StringToken(parts: NonEmptyList[StringFragment])
      extends Token(TokenCategory.StringToken)

  object StringToken extends TokenFactory[StringToken] {
    override val category: TokenCategory = TokenCategory.StringToken
  }

  final case class IntToken(value: BigInt)
      extends Token(TokenCategory.IntToken)

  object IntToken extends TokenFactory[IntToken] {
    override val category: TokenCategory = TokenCategory.IntToken
  }

  final case class Identifier(name: String) extends Token(TokenCategory.Identifier)

  object Identifier extends TokenFactory[Identifier] {
    override val category: TokenCategory = TokenCategory.Identifier
  }

  case object NewLine extends Token(TokenCategory.NewLine)
  case object Semicolon extends Token(TokenCategory.SYM_SEMICOLON)

  case object KW_ARGON_BUILTIN extends Token(TokenCategory.KW_ARGON_BUILTIN)

  case object KW_DEF extends Token(TokenCategory.KW_DEF)
  case object KW_PROC extends Token(TokenCategory.KW_PROC)
  case object KW_DO extends Token(TokenCategory.KW_DO)
  case object KW_END extends Token(TokenCategory.KW_END)
  case object KW_VAR extends Token(TokenCategory.KW_VAR)
  case object KW_VAL extends Token(TokenCategory.KW_VAL)
  case object KW_RECORD extends Token(TokenCategory.KW_RECORD)
  case object KW_WITH extends Token(TokenCategory.KW_WITH)
  case object KW_TRUE extends Token(TokenCategory.KW_TRUE)
  case object KW_FALSE extends Token(TokenCategory.KW_FALSE)
  case object KW_AS extends Token(TokenCategory.KW_AS)
  case object KW_IMPORT extends Token(TokenCategory.KW_IMPORT)
  case object KW_EXPORT extends Token(TokenCategory.KW_EXPORT)

  case object KW_PUBLIC extends Token(TokenCategory.KW_PUBLIC) with ModifierToken(Modifier.Public)
  case object KW_PROTECTED extends Token(TokenCategory.KW_PROTECTED) with ModifierToken(Modifier.Protected)
  case object KW_PRIVATE extends Token(TokenCategory.KW_PRIVATE) with ModifierToken(Modifier.Private)
  case object KW_INTERNAL extends Token(TokenCategory.KW_INTERNAL) with ModifierToken(Modifier.Internal)

  case object KW_IF extends Token(TokenCategory.KW_IF)
  case object KW_THEN extends Token(TokenCategory.KW_THEN)
  case object KW_ELSE extends Token(TokenCategory.KW_ELSE)
  case object KW_ELSIF extends Token(TokenCategory.KW_ELSIF)

  case object KW_ERASED extends Token(TokenCategory.KW_ERASED) with ModifierToken(Modifier.Erased)
  case object KW_PROOF extends Token(TokenCategory.KW_PROOF) with ModifierToken(Modifier.Proof)
  case object KW_INLINE extends Token(TokenCategory.KW_INLINE) with ModifierToken(Modifier.Inline)

  case object KW_TYPE extends Token(TokenCategory.KW_TYPE)
  case object KW_BIGTYPE extends Token(TokenCategory.KW_BIGTYPE)
  case object KW_UNDERSCORE extends Token(TokenCategory.KW_UNDERSCORE)
  case object KW_EXTERN extends Token(TokenCategory.KW_EXTERN)
  case object KW_RAISE extends Token(TokenCategory.KW_RAISE)
  case object KW_BEGIN extends Token(TokenCategory.KW_BEGIN)
  case object KW_RESCUE extends Token(TokenCategory.KW_RESCUE)
  case object KW_FINALLY extends Token(TokenCategory.KW_FINALLY)
  case object KW_REQUIRES extends Token(TokenCategory.KW_REQUIRES)
  case object KW_ENSURES extends Token(TokenCategory.KW_ENSURES)
  case object KW_MAINTAINS extends Token(TokenCategory.KW_MAINTAINS)
  case object KW_ASSERT extends Token(TokenCategory.KW_ASSERT)
  case object KW_SUMMON extends Token(TokenCategory.KW_SUMMON)
  case object KW_EXTENSION extends Token(TokenCategory.KW_EXTENSION)
  case object KW_INVERSE extends Token(TokenCategory.KW_INVERSE)
  case object KW_UPDATE extends Token(TokenCategory.KW_UPDATE)
  case object KW_OPERATOR extends Token(TokenCategory.KW_OPERATOR)
  case object KW_UNARY extends Token(TokenCategory.KW_UNARY)

  case object OP_LOGICAL_AND extends Token(TokenCategory.OP_LOGICAL_AND) with BinaryOperatorToken(BinaryOperator.LogicalAnd)

  case object OP_LOGICAL_OR extends Token(TokenCategory.OP_LOGICAL_OR) with BinaryOperatorToken(BinaryOperator.LogicalOr)

  case object OP_EQUALS extends Token(TokenCategory.OP_EQUALS) with BinaryOperatorToken(BinaryOperator.Equal)

  case object OP_NOTEQUALS extends Token(TokenCategory.OP_NOTEQUALS) with BinaryOperatorToken(BinaryOperator.NotEqual)

  case object OP_LESSTHANEQ extends Token(TokenCategory.OP_LESSTHANEQ) with BinaryOperatorToken(BinaryOperator.LessThanEq)

  case object OP_GREATERTHANEQ extends Token(TokenCategory.OP_GREATERTHANEQ) with BinaryOperatorToken(BinaryOperator.GreaterThanEq)

  case object OP_SHIFTLEFT extends Token(TokenCategory.OP_SHIFTLEFT) with BinaryOperatorToken(BinaryOperator.ShiftLeft)

  case object OP_SHIFTRIGHT extends Token(TokenCategory.OP_SHIFTRIGHT) with BinaryOperatorToken(BinaryOperator.ShiftRight)

  case object OP_ASSIGN extends Token(TokenCategory.OP_ASSIGN)
  case object OP_DOT extends Token(TokenCategory.SYM_DOT)
  case object OP_DOTDOT extends Token(TokenCategory.SYM_DOT)
  case object OP_COMMA extends Token(TokenCategory.SYM_COMMA)
  case object OP_OPENPAREN extends Token(TokenCategory.SYM_OPENPAREN)
  case object OP_CLOSEPAREN extends Token(TokenCategory.SYM_CLOSEPAREN)
  case object OP_OPENBRACKET extends Token(TokenCategory.SYM_OPENBRACKET)
  case object OP_CLOSEBRACKET extends Token(TokenCategory.SYM_CLOSEBRACKET)
  case object OP_OPENCURLY extends Token(TokenCategory.SYM_OPENCURLY)
  case object OP_CLOSECURLY extends Token(TokenCategory.SYM_CLOSECURLY)
  case object OP_STARSTAR extends Token(TokenCategory.OP_STARSTAR)

  case object OP_LOGICAL_NOT extends Token(TokenCategory.OP_LOGICAL_NOT) with UnaryOperatorToken(UnaryOperator.LogicalNot)


  case object OP_PLUS extends Token(TokenCategory.OP_PLUS) with BinaryOperatorToken(BinaryOperator.Plus) with UnaryOperatorToken(UnaryOperator.Plus)
  case object OP_MINUS extends Token(TokenCategory.OP_MINUS) with BinaryOperatorToken(BinaryOperator.Minus) with UnaryOperatorToken(UnaryOperator.Minus)

  case object OP_STAR extends Token(TokenCategory.OP_MUL) with BinaryOperatorToken(BinaryOperator.Mul)
  case object OP_MUL extends Token(TokenCategory.OP_MUL) with BinaryOperatorToken(BinaryOperator.Mul)

  case object OP_SLASH extends Token(TokenCategory.OP_DIV) with BinaryOperatorToken(BinaryOperator.Div)
  case object OP_DIV extends Token(TokenCategory.OP_DIV) with BinaryOperatorToken(BinaryOperator.Div)

  case object OP_BITAND extends Token(TokenCategory.OP_BITAND) with BinaryOperatorToken(BinaryOperator.BitAnd)

  case object OP_BITOR extends Token(TokenCategory.OP_BITOR) with BinaryOperatorToken(BinaryOperator.BitOr)

  case object OP_BITXOR extends Token(TokenCategory.OP_BITXOR) with BinaryOperatorToken(BinaryOperator.BitXOr)

  case object OP_BITNOT extends Token(TokenCategory.OP_BITNOT) with UnaryOperatorToken(UnaryOperator.BitNot)

  case object OP_LESSTHAN extends Token(TokenCategory.OP_LESSTHAN) with BinaryOperatorToken(BinaryOperator.LessThan)

  case object OP_GREATERTHAN extends Token(TokenCategory.OP_GREATERTHAN) with BinaryOperatorToken(BinaryOperator.GreaterThan)

  case object OP_COLON extends Token(TokenCategory.SYM_COLON)
  case object OP_LAMBDA_TYPE extends Token(TokenCategory.OP_LAMBDA_TYPE)
  case object OP_LAMBDA extends Token(TokenCategory.OP_LAMBDA)

  case object OP_CONCAT extends Token(TokenCategory.OP_CONCAT) with BinaryOperatorToken(BinaryOperator.Concat)

  case object OP_FUNCTION_RESULT_VALUE extends Token(TokenCategory.SYM_FUNCTION_RESULT_VALUE)

  case object OP_PROP_EQUAL extends Token(TokenCategory.OP_PROP_EQUAL) with BinaryOperatorToken(BinaryOperator.PropEqual)

  case object OP_PROP_DISJUNCTION extends Token(TokenCategory.OP_PROP_DISJUNCTION) with BinaryOperatorToken(BinaryOperator.PropDisjunction)

  case object OP_PROP_CONJUNCTION extends Token(TokenCategory.OP_PROP_CONJUNCTION) with BinaryOperatorToken(BinaryOperator.PropConjunction)
}
