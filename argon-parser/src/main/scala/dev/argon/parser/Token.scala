package dev.argon.parser

import dev.argon.ast.*
import dev.argon.util.*
import cats.data.NonEmptySeq

private[parser] sealed abstract class Token derives CanEqual {
  val category: TokenCategory
}

private[parser] object Token {
  sealed abstract class TokenWithCategory[TC <: TokenCategory](val category: TC) extends Token
  
  trait TokenFactory[TToken <: Token] {
    val category: TokenCategory
  }
  
  case object EndOfFile extends TokenWithCategory(TokenCategory.EndOfFile)

  sealed trait BinaryOperatorToken[O <: BinaryOperator](val binOperator: O) {
    self: Token =>
  } 

  sealed trait UnaryOperatorToken[O <: UnaryOperator](val unOperator: O) {
    self: Token =>
  }

  sealed trait ModifierToken(val modifier: Modifier) {
    self: Token =>
  }

  case object StringStart extends TokenWithCategory(TokenCategory.StringStart)
  case object StringEnd extends TokenWithCategory(TokenCategory.StringEnd)
  final case class StringText(text: String) extends TokenWithCategory(TokenCategory.StringText)
  case object StringInterpolationStart extends TokenWithCategory(TokenCategory.StringInterpolationStart)

  final case class IntToken(value: BigInt)
      extends TokenWithCategory(TokenCategory.IntToken)

  object IntToken extends TokenFactory[IntToken] {
    override val category: TokenCategory = TokenCategory.IntToken
  }

  final case class Identifier(name: String) extends TokenWithCategory(TokenCategory.Identifier)

  object Identifier extends TokenFactory[Identifier] {
    override val category: TokenCategory = TokenCategory.Identifier
  }

  case object NewLine extends TokenWithCategory(TokenCategory.NewLine)
  case object Semicolon extends TokenWithCategory(TokenCategory.SYM_SEMICOLON)

  case object KW_ARGON_BUILTIN extends TokenWithCategory(TokenCategory.KW_ARGON_BUILTIN)

  case object KW_DEF extends TokenWithCategory(TokenCategory.KW_DEF)
  case object KW_PROC extends TokenWithCategory(TokenCategory.KW_PROC)
  case object KW_DO extends TokenWithCategory(TokenCategory.KW_DO)
  case object KW_END extends TokenWithCategory(TokenCategory.KW_END)
  case object KW_LET extends TokenWithCategory(TokenCategory.KW_LET)
  case object KW_VAL extends TokenWithCategory(TokenCategory.KW_VAL)
  case object KW_MUT extends TokenWithCategory(TokenCategory.KW_MUT)
  case object KW_MODULE extends TokenWithCategory(TokenCategory.KW_MODULE)
  case object KW_RECORD extends TokenWithCategory(TokenCategory.KW_RECORD)
  case object KW_ENUM extends TokenWithCategory(TokenCategory.KW_ENUM)
  case object KW_WITH extends TokenWithCategory(TokenCategory.KW_WITH)
  case object KW_TRUE extends TokenWithCategory(TokenCategory.KW_TRUE)
  case object KW_FALSE extends TokenWithCategory(TokenCategory.KW_FALSE)
  case object KW_AS extends TokenWithCategory(TokenCategory.KW_AS)
  case object KW_IS extends TokenWithCategory(TokenCategory.KW_IS)
  case object KW_IMPORT extends TokenWithCategory(TokenCategory.KW_IMPORT)
  case object KW_EXPORT extends TokenWithCategory(TokenCategory.KW_EXPORT)

  case object KW_PUBLIC extends TokenWithCategory(TokenCategory.KW_PUBLIC) with ModifierToken(Modifier.Public)
  case object KW_PROTECTED extends TokenWithCategory(TokenCategory.KW_PROTECTED) with ModifierToken(Modifier.Protected)
  case object KW_PRIVATE extends TokenWithCategory(TokenCategory.KW_PRIVATE) with ModifierToken(Modifier.Private)
  case object KW_INTERNAL extends TokenWithCategory(TokenCategory.KW_INTERNAL) with ModifierToken(Modifier.Internal)

  case object KW_IF extends TokenWithCategory(TokenCategory.KW_IF)
  case object KW_THEN extends TokenWithCategory(TokenCategory.KW_THEN)
  case object KW_ELSE extends TokenWithCategory(TokenCategory.KW_ELSE)
  case object KW_ELSIF extends TokenWithCategory(TokenCategory.KW_ELSIF)
  case object KW_MATCH extends TokenWithCategory(TokenCategory.KW_MATCH)
  case object KW_CASE extends TokenWithCategory(TokenCategory.KW_CASE)

  case object KW_ERASED extends TokenWithCategory(TokenCategory.KW_ERASED) with ModifierToken(Modifier.Erased)
  case object KW_WITNESS extends TokenWithCategory(TokenCategory.KW_WITNESS) with ModifierToken(Modifier.Witness)
  case object KW_INLINE extends TokenWithCategory(TokenCategory.KW_INLINE) with ModifierToken(Modifier.Inline)
  
  case object KW_ABSTRACT extends TokenWithCategory(TokenCategory.KW_ABSTRACT)
  

  case object KW_TYPE extends TokenWithCategory(TokenCategory.KW_TYPE)
  case object KW_BIGTYPE extends TokenWithCategory(TokenCategory.KW_BIGTYPE)
  case object KW_UNDERSCORE extends TokenWithCategory(TokenCategory.KW_UNDERSCORE)
  case object KW_EXTERN extends TokenWithCategory(TokenCategory.KW_EXTERN)
  case object KW_RAISE extends TokenWithCategory(TokenCategory.KW_RAISE)
  case object KW_BEGIN extends TokenWithCategory(TokenCategory.KW_BEGIN)
  case object KW_RESCUE extends TokenWithCategory(TokenCategory.KW_RESCUE)
  case object KW_FINALLY extends TokenWithCategory(TokenCategory.KW_FINALLY)
  case object KW_REQUIRES extends TokenWithCategory(TokenCategory.KW_REQUIRES)
  case object KW_ENSURES extends TokenWithCategory(TokenCategory.KW_ENSURES)
  case object KW_MAINTAINS extends TokenWithCategory(TokenCategory.KW_MAINTAINS)
  case object KW_ASSERT extends TokenWithCategory(TokenCategory.KW_ASSERT)
  case object KW_SUMMON extends TokenWithCategory(TokenCategory.KW_SUMMON)
  case object KW_EXTENSION extends TokenWithCategory(TokenCategory.KW_EXTENSION)
  case object KW_INVERSE extends TokenWithCategory(TokenCategory.KW_INVERSE)
  case object KW_UPDATE extends TokenWithCategory(TokenCategory.KW_UPDATE)
  case object KW_OPERATOR extends TokenWithCategory(TokenCategory.KW_OPERATOR)
  case object KW_UNARY extends TokenWithCategory(TokenCategory.KW_UNARY)
  case object KW_BOXED extends TokenWithCategory(TokenCategory.KW_BOXED)
  case object KW_BOX extends TokenWithCategory(TokenCategory.KW_BOX)
  case object KW_UNBOX extends TokenWithCategory(TokenCategory.KW_UNBOX)
  case object KW_FN extends TokenWithCategory(TokenCategory.KW_FN)

  case object OP_LOGICAL_AND extends TokenWithCategory(TokenCategory.OP_LOGICAL_AND) with BinaryOperatorToken(BinaryOperator.LogicalAnd)

  case object OP_LOGICAL_OR extends TokenWithCategory(TokenCategory.OP_LOGICAL_OR) with BinaryOperatorToken(BinaryOperator.LogicalOr)

  case object OP_EQUALS extends TokenWithCategory(TokenCategory.OP_EQUALS) with BinaryOperatorToken(BinaryOperator.Equal)

  case object OP_NOTEQUALS extends TokenWithCategory(TokenCategory.OP_NOTEQUALS) with BinaryOperatorToken(BinaryOperator.NotEqual)

  case object OP_LESSTHANEQ extends TokenWithCategory(TokenCategory.OP_LESSTHANEQ) with BinaryOperatorToken(BinaryOperator.LessThanEq)

  case object OP_GREATERTHANEQ extends TokenWithCategory(TokenCategory.OP_GREATERTHANEQ) with BinaryOperatorToken(BinaryOperator.GreaterThanEq)

  case object OP_SHIFTLEFT extends TokenWithCategory(TokenCategory.OP_SHIFTLEFT) with BinaryOperatorToken(BinaryOperator.ShiftLeft)

  case object OP_SHIFTRIGHT extends TokenWithCategory(TokenCategory.OP_SHIFTRIGHT) with BinaryOperatorToken(BinaryOperator.ShiftRight)

  case object OP_ASSIGN extends TokenWithCategory(TokenCategory.OP_ASSIGN)
  case object OP_DOTDOT extends TokenWithCategory(TokenCategory.OP_DOTDOT)
  case object OP_STARSTAR extends TokenWithCategory(TokenCategory.OP_STARSTAR)

  case object OP_LOGICAL_NOT extends TokenWithCategory(TokenCategory.OP_LOGICAL_NOT) with UnaryOperatorToken(UnaryOperator.LogicalNot)


  case object OP_PLUS extends TokenWithCategory(TokenCategory.OP_PLUS) with BinaryOperatorToken(BinaryOperator.Plus) with UnaryOperatorToken(UnaryOperator.Plus)
  case object OP_MINUS extends TokenWithCategory(TokenCategory.OP_MINUS) with BinaryOperatorToken(BinaryOperator.Minus) with UnaryOperatorToken(UnaryOperator.Minus)
  case object OP_STAR extends TokenWithCategory(TokenCategory.OP_STAR) with BinaryOperatorToken(BinaryOperator.Mul)
  case object OP_MUL extends TokenWithCategory(TokenCategory.OP_MUL) with BinaryOperatorToken(BinaryOperator.Mul)
  case object OP_SLASH extends TokenWithCategory(TokenCategory.OP_SLASH) with BinaryOperatorToken(BinaryOperator.Div)
  case object OP_DIV extends TokenWithCategory(TokenCategory.OP_DIV) with BinaryOperatorToken(BinaryOperator.Div)

  case object OP_BITAND extends TokenWithCategory(TokenCategory.OP_BITAND) with BinaryOperatorToken(BinaryOperator.BitAnd)

  case object OP_BITOR extends TokenWithCategory(TokenCategory.OP_BITOR) with BinaryOperatorToken(BinaryOperator.BitOr)

  case object OP_BITXOR extends TokenWithCategory(TokenCategory.OP_BITXOR) with BinaryOperatorToken(BinaryOperator.BitXOr)

  case object OP_BITNOT extends TokenWithCategory(TokenCategory.OP_BITNOT) with UnaryOperatorToken(UnaryOperator.BitNot)

  case object OP_LESSTHAN extends TokenWithCategory(TokenCategory.OP_LESSTHAN) with BinaryOperatorToken(BinaryOperator.LessThan)

  case object OP_GREATERTHAN extends TokenWithCategory(TokenCategory.OP_GREATERTHAN) with BinaryOperatorToken(BinaryOperator.GreaterThan)

  case object SYM_COLONCOLON extends TokenWithCategory(TokenCategory.SYM_COLONCOLON)
  case object OP_ARROW extends TokenWithCategory(TokenCategory.OP_ARROW)
  case object OP_FAT_ARROW extends TokenWithCategory(TokenCategory.OP_FAT_ARROW)

  case object OP_CONCAT extends TokenWithCategory(TokenCategory.OP_CONCAT) with BinaryOperatorToken(BinaryOperator.Concat)

  case object OP_PROP_EQUAL extends TokenWithCategory(TokenCategory.OP_PROP_EQUAL) with BinaryOperatorToken(BinaryOperator.PropEqual)

  case object OP_PROP_DISJUNCTION extends TokenWithCategory(TokenCategory.OP_PROP_DISJUNCTION) with BinaryOperatorToken(BinaryOperator.PropDisjunction)

  case object OP_PROP_CONJUNCTION extends TokenWithCategory(TokenCategory.OP_PROP_CONJUNCTION) with BinaryOperatorToken(BinaryOperator.PropConjunction)


  case object SYM_COLON extends TokenWithCategory(TokenCategory.SYM_COLON)
  case object SYM_DOT extends TokenWithCategory(TokenCategory.SYM_DOT)
  case object SYM_COMMA extends TokenWithCategory(TokenCategory.SYM_COMMA)
  case object SYM_OPENPAREN extends TokenWithCategory(TokenCategory.SYM_OPENPAREN)
  case object SYM_CLOSEPAREN extends TokenWithCategory(TokenCategory.SYM_CLOSEPAREN)
  case object SYM_OPENBRACKET extends TokenWithCategory(TokenCategory.SYM_OPENBRACKET)
  case object SYM_CLOSEBRACKET extends TokenWithCategory(TokenCategory.SYM_CLOSEBRACKET)
  case object SYM_OPENCURLY extends TokenWithCategory(TokenCategory.SYM_OPENCURLY)
  case object SYM_CLOSECURLY extends TokenWithCategory(TokenCategory.SYM_CLOSECURLY)
  case object SYM_PIPE extends TokenWithCategory(TokenCategory.SYM_PIPE)
  case object SYM_AT extends TokenWithCategory(TokenCategory.SYM_AT)
}
