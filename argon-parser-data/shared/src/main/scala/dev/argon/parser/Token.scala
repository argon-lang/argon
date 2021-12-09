package dev.argon.parser

import dev.argon.util._

sealed trait Token derives CanEqual {
  def category: TokenCategory
}

object Token {

  class TokenWithCategory[+TCategory <: TokenCategory](val category: TCategory) { self: Token => }

  trait TokenFactory[TToken <: Token] {
    val category: TokenCategory
  }

  sealed trait OperatorToken extends Token

  sealed trait BinaryOperatorToken extends OperatorToken {
    def binaryOperator: BinaryOperator
  }

  sealed trait UnaryOperatorToken extends OperatorToken {
    def unaryOperator: UnaryOperator
  }

  sealed trait ModifierToken extends Token {
    def modifier: Modifier
  }

  final case class StringToken(parts: NonEmptyList[StringToken.Part])
      extends TokenWithCategory(TokenCategory.StringToken) with Token

  object StringToken extends TokenFactory[StringToken] {
    override val category: TokenCategory = TokenCategory.StringToken
    sealed trait Part
    final case class StringPart(str: WithSource[String]) extends Part
    final case class ExprPart(format: Option[WithSource[String]], expr: WithSource[Expr]) extends Part
  }

  final case class IntToken(sign: Int, base: BigInt, digits: Vector[BigInt])
      extends TokenWithCategory(TokenCategory.IntToken) with Token

  object IntToken extends TokenFactory[IntToken] {
    override val category: TokenCategory = TokenCategory.IntToken
  }

  final case class Identifier(name: String) extends TokenWithCategory(TokenCategory.Identifier) with Token

  object Identifier extends TokenFactory[Identifier] {
    override val category: TokenCategory = TokenCategory.Identifier
  }

  case object NewLine extends TokenWithCategory(TokenCategory.NewLine) with Token
  case object Semicolon extends TokenWithCategory(TokenCategory.Semicolon) with Token

  case object KW_DEF extends TokenWithCategory(TokenCategory.KW_DEF) with Token
  case object KW_PROC extends TokenWithCategory(TokenCategory.KW_PROC) with Token
  case object KW_INSTANCE extends TokenWithCategory(TokenCategory.KW_INSTANCE) with Token
  case object KW_CONSTRUCTOR extends TokenWithCategory(TokenCategory.KW_CONSTRUCTOR) with Token
  case object KW_END extends TokenWithCategory(TokenCategory.KW_END) with Token
  case object KW_DO extends TokenWithCategory(TokenCategory.KW_DO) with Token
  case object KW_VAR extends TokenWithCategory(TokenCategory.KW_VAR) with Token
  case object KW_VAL extends TokenWithCategory(TokenCategory.KW_VAL) with Token
  case object KW_TRUE extends TokenWithCategory(TokenCategory.KW_TRUE) with Token
  case object KW_FALSE extends TokenWithCategory(TokenCategory.KW_FALSE) with Token
  case object KW_AS extends TokenWithCategory(TokenCategory.KW_AS) with Token
  case object KW_NAMESPACE extends TokenWithCategory(TokenCategory.KW_NAMESPACE) with Token
  case object KW_IMPORT extends TokenWithCategory(TokenCategory.KW_IMPORT) with Token
  case object KW_TRAIT extends TokenWithCategory(TokenCategory.KW_TRAIT) with Token
  case object KW_STATIC extends TokenWithCategory(TokenCategory.KW_STATIC) with Token
  case object KW_DATA extends TokenWithCategory(TokenCategory.KW_DATA) with Token

  case object KW_PUBLIC extends TokenWithCategory(TokenCategory.KW_PUBLIC) with ModifierToken {
    override def modifier: Modifier = PublicModifier
  }

  case object KW_PROTECTED extends TokenWithCategory(TokenCategory.KW_PROTECTED) with ModifierToken {
    override def modifier: Modifier = ProtectedModifier
  }

  case object KW_PRIVATE extends TokenWithCategory(TokenCategory.KW_PRIVATE) with ModifierToken {
    override def modifier: Modifier = PrivateModifier
  }

  case object KW_INTERNAL extends TokenWithCategory(TokenCategory.KW_INTERNAL) with ModifierToken {
    override def modifier: Modifier = InternalModifier
  }

  case object KW_BASE extends TokenWithCategory(TokenCategory.KW_BASE) with Token
  case object KW_IF extends TokenWithCategory(TokenCategory.KW_IF) with Token
  case object KW_THEN extends TokenWithCategory(TokenCategory.KW_THEN) with Token
  case object KW_ELSE extends TokenWithCategory(TokenCategory.KW_ELSE) with Token
  case object KW_ELSIF extends TokenWithCategory(TokenCategory.KW_ELSIF) with Token

  case object KW_OPEN extends TokenWithCategory(TokenCategory.KW_OPEN) with ModifierToken {
    override def modifier: Modifier = OpenModifier
  }

  case object KW_SEALED extends TokenWithCategory(TokenCategory.KW_SEALED) with ModifierToken {
    override def modifier: Modifier = SealedModifier
  }

  case object KW_VIRTUAL extends TokenWithCategory(TokenCategory.KW_VIRTUAL) with ModifierToken {
    override def modifier: Modifier = VirtualModifier
  }

  case object KW_ABSTRACT extends TokenWithCategory(TokenCategory.KW_ABSTRACT) with ModifierToken {
    override def modifier: Modifier = AbstractModifier
  }

  case object KW_OVERRIDE extends TokenWithCategory(TokenCategory.KW_OVERRIDE) with ModifierToken {
    override def modifier: Modifier = OverrideModifier
  }

  case object KW_FINAL extends TokenWithCategory(TokenCategory.KW_FINAL) with Token
  case object KW_TYPE extends TokenWithCategory(TokenCategory.KW_TYPE) with Token
  case object KW_METATYPE extends TokenWithCategory(TokenCategory.KW_METATYPE) with Token
  case object KW_MATCH extends TokenWithCategory(TokenCategory.KW_MATCH) with Token
  case object KW_CASE extends TokenWithCategory(TokenCategory.KW_CASE) with Token
  case object KW_CLASS extends TokenWithCategory(TokenCategory.KW_CLASS) with Token
  case object KW_NEW extends TokenWithCategory(TokenCategory.KW_NEW) with Token
  case object KW_FIELD extends TokenWithCategory(TokenCategory.KW_FIELD) with Token
  case object KW_INITIALIZE extends TokenWithCategory(TokenCategory.KW_INITIALIZE) with Token
  case object KW_UNDERSCORE extends TokenWithCategory(TokenCategory.KW_UNDERSCORE) with Token
  case object KW_GC extends TokenWithCategory(TokenCategory.KW_GC) with Token
  case object KW_STRUCT extends TokenWithCategory(TokenCategory.KW_STRUCT) with Token
  case object KW_STACK extends TokenWithCategory(TokenCategory.KW_STACK) with Token
  case object KW_ANY extends TokenWithCategory(TokenCategory.KW_ANY) with Token
  case object KW_VALUETYPE extends TokenWithCategory(TokenCategory.KW_VALUETYPE) with Token
  case object KW_EXTERN extends TokenWithCategory(TokenCategory.KW_EXTERN) with Token
  case object KW_RAISE extends TokenWithCategory(TokenCategory.KW_RAISE) with Token
  case object KW_BEGIN extends TokenWithCategory(TokenCategory.KW_BEGIN) with Token
  case object KW_RESCUE extends TokenWithCategory(TokenCategory.KW_RESCUE) with Token
  case object KW_ENSURE extends TokenWithCategory(TokenCategory.KW_ENSURE) with Token
  case object KW_ERASED extends TokenWithCategory(TokenCategory.KW_ERASED) with Token
  case object KW_REQUIRES extends TokenWithCategory(TokenCategory.KW_REQUIRES) with Token
  case object KW_ENSURES extends TokenWithCategory(TokenCategory.KW_ENSURES) with Token
  case object KW_MAINTAINS extends TokenWithCategory(TokenCategory.KW_MAINTAINS) with Token
  case object KW_ASSERT extends TokenWithCategory(TokenCategory.KW_ASSERT) with Token
  case object KW_GIVEN extends TokenWithCategory(TokenCategory.KW_GIVEN) with Token
  case object KW_EXTENSION extends TokenWithCategory(TokenCategory.KW_EXTENSION) with Token
  case object KW_INVERSE extends TokenWithCategory(TokenCategory.KW_INVERSE) with Token
  case object KW_UPDATE extends TokenWithCategory(TokenCategory.KW_UPDATE) with Token

  case object OP_BOOLAND extends TokenWithCategory(TokenCategory.OP_BOOLAND) with BinaryOperatorToken {
    override def binaryOperator: BinaryOperator = BinaryOperator.BoolAnd
  }

  case object OP_BOOLOR extends TokenWithCategory(TokenCategory.OP_BOOLOR) with BinaryOperatorToken {
    override def binaryOperator: BinaryOperator = BinaryOperator.BoolOr
  }

  case object OP_EQUALS extends TokenWithCategory(TokenCategory.OP_EQUALS) with BinaryOperatorToken {
    override def binaryOperator: BinaryOperator = BinaryOperator.Equal
  }

  case object OP_NOTEQUALS extends TokenWithCategory(TokenCategory.OP_NOTEQUALS) with BinaryOperatorToken {
    override def binaryOperator: BinaryOperator = BinaryOperator.NotEqual
  }

  case object OP_LESSTHANEQ extends TokenWithCategory(TokenCategory.OP_LESSTHANEQ) with BinaryOperatorToken {
    override def binaryOperator: BinaryOperator = BinaryOperator.LessThanEq
  }

  case object OP_GREATERTHANEQ extends TokenWithCategory(TokenCategory.OP_GREATERTHANEQ) with BinaryOperatorToken {
    override def binaryOperator: BinaryOperator = BinaryOperator.GreaterThanEq
  }

  case object OP_SHIFTLEFT extends TokenWithCategory(TokenCategory.OP_SHIFTLEFT) with BinaryOperatorToken {
    override def binaryOperator: BinaryOperator = BinaryOperator.ShiftLeft
  }

  case object OP_SHIFTRIGHT extends TokenWithCategory(TokenCategory.OP_SHIFTRIGHT) with BinaryOperatorToken {
    override def binaryOperator: BinaryOperator = BinaryOperator.ShiftRight
  }

  case object OP_ASSIGN extends TokenWithCategory(TokenCategory.OP_ASSIGN) with Token
  case object OP_DOT extends TokenWithCategory(TokenCategory.OP_DOT) with Token
  case object OP_DOTDOT extends TokenWithCategory(TokenCategory.OP_DOT) with Token
  case object OP_COMMA extends TokenWithCategory(TokenCategory.OP_COMMA) with Token
  case object OP_OPENPAREN extends TokenWithCategory(TokenCategory.OP_OPENPAREN) with Token
  case object OP_CLOSEPAREN extends TokenWithCategory(TokenCategory.OP_CLOSEPAREN) with Token
  case object OP_OPENBRACKET extends TokenWithCategory(TokenCategory.OP_OPENBRACKET) with Token
  case object OP_CLOSEBRACKET extends TokenWithCategory(TokenCategory.OP_CLOSEBRACKET) with Token
  case object OP_OPENCURLY extends TokenWithCategory(TokenCategory.OP_OPENCURLY) with Token
  case object OP_CLOSECURLY extends TokenWithCategory(TokenCategory.OP_CLOSECURLY) with Token

  case object OP_BOOLNOT extends TokenWithCategory(TokenCategory.OP_BOOLNOT) with UnaryOperatorToken {
    override def unaryOperator: UnaryOperator = UnaryOperator.BoolNot
  }

  case object OP_ADD extends TokenWithCategory(TokenCategory.OP_ADD) with BinaryOperatorToken with UnaryOperatorToken {
    override def binaryOperator: BinaryOperator = BinaryOperator.Add
    override def unaryOperator: UnaryOperator = UnaryOperator.UnaryPlus
  }

  case object OP_SUB extends TokenWithCategory(TokenCategory.OP_SUB) with BinaryOperatorToken with UnaryOperatorToken {
    override def binaryOperator: BinaryOperator = BinaryOperator.Sub
    override def unaryOperator: UnaryOperator = UnaryOperator.UnaryMinus
  }

  sealed class MultiplicationOperator extends TokenWithCategory(TokenCategory.OP_MUL) with BinaryOperatorToken {
    override def binaryOperator: BinaryOperator = BinaryOperator.Mul
  }

  object MultiplicationOperator extends TokenFactory[MultiplicationOperator] {
    override val category: TokenCategory = TokenCategory.OP_MUL
  }

  case object OP_STAR extends MultiplicationOperator
  case object OP_MUL extends MultiplicationOperator

  sealed class DivisionOperator extends TokenWithCategory(TokenCategory.OP_DIV) with BinaryOperatorToken {
    override def binaryOperator: BinaryOperator = BinaryOperator.Div
  }

  object DivisionOperator extends TokenFactory[DivisionOperator] {
    override val category: TokenCategory = TokenCategory.OP_DIV
  }

  case object OP_SLASH extends DivisionOperator
  case object OP_DIV extends DivisionOperator

  case object OP_BITAND extends TokenWithCategory(TokenCategory.OP_BITAND) with BinaryOperatorToken {
    override def binaryOperator: BinaryOperator = BinaryOperator.BitAnd
  }

  case object OP_BITOR extends TokenWithCategory(TokenCategory.OP_BITOR) with BinaryOperatorToken {
    override def binaryOperator: BinaryOperator = BinaryOperator.BitOr
  }

  case object OP_BITXOR extends TokenWithCategory(TokenCategory.OP_BITXOR) with BinaryOperatorToken {
    override def binaryOperator: BinaryOperator = BinaryOperator.BitXOr
  }

  case object OP_BITNOT extends TokenWithCategory(TokenCategory.OP_BITNOT) with UnaryOperatorToken {
    override def unaryOperator: UnaryOperator = UnaryOperator.BitNot
  }

  case object OP_LESSTHAN extends TokenWithCategory(TokenCategory.OP_LESSTHAN) with BinaryOperatorToken {
    override def binaryOperator: BinaryOperator = BinaryOperator.LessThan
  }

  case object OP_GREATERTHAN extends TokenWithCategory(TokenCategory.OP_GREATERTHAN) with BinaryOperatorToken {
    override def binaryOperator: BinaryOperator = BinaryOperator.GreaterThan
  }

  case object OP_COLON extends TokenWithCategory(TokenCategory.OP_COLON) with Token
  case object OP_SUBTYPE extends TokenWithCategory(TokenCategory.OP_SUBTYPE) with Token
  case object OP_SUPERTYPE extends TokenWithCategory(TokenCategory.OP_SUPERTYPE) with Token
  case object OP_LAMBDA_TYPE extends TokenWithCategory(TokenCategory.OP_LAMBDA_TYPE) with Token
  case object OP_LAMBDA extends TokenWithCategory(TokenCategory.OP_LAMBDA) with Token

  case object OP_UNION extends TokenWithCategory(TokenCategory.OP_UNION) with BinaryOperatorToken {
    override def binaryOperator: BinaryOperator = BinaryOperator.Union
  }

  case object OP_INTERSECTION extends TokenWithCategory(TokenCategory.OP_INTERSECTION) with BinaryOperatorToken {
    override def binaryOperator: BinaryOperator = BinaryOperator.Intersection
  }

  case object OP_CONCAT extends TokenWithCategory(TokenCategory.OP_CONCAT) with BinaryOperatorToken {
    override def binaryOperator: BinaryOperator = BinaryOperator.Concat
  }

}
