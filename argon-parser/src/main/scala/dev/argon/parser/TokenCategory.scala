package dev.argon.parser

sealed trait TokenCategory derives CanEqual {
  type TokenType = this.type
}

object TokenCategory {
  final case class Wrap[T](value: TokenCategory { type TokenType = T })

  case object EndOfFile extends TokenCategory
  
  case object StringStart extends TokenCategory
  case object StringEnd extends TokenCategory
  case object StringText extends TokenCategory
  case object StringInterpolationStart extends TokenCategory
  
  case object IntToken extends TokenCategory

  case object Identifier extends TokenCategory

  case object NewLine extends TokenCategory
  
  case object KW_ARGON_BUILTIN extends TokenCategory

  case object KW_DEF extends TokenCategory
  case object KW_PROC extends TokenCategory
  case object KW_DO extends TokenCategory
  case object KW_END extends TokenCategory
  case object KW_LET extends TokenCategory
  case object KW_VAL extends TokenCategory
  case object KW_MUT extends TokenCategory
  case object KW_MODULE extends TokenCategory
  case object KW_RECORD extends TokenCategory
  case object KW_ENUM extends TokenCategory
  case object KW_TRAIT extends TokenCategory
  case object KW_INSTANCE extends TokenCategory
  case object KW_NEW extends TokenCategory
  case object KW_WITH extends TokenCategory
  case object KW_TRUE extends TokenCategory
  case object KW_FALSE extends TokenCategory
  case object KW_AS extends TokenCategory
  case object KW_IS extends TokenCategory
  case object KW_IMPORT extends TokenCategory
  case object KW_EXPORT extends TokenCategory
  case object KW_PUBLIC extends TokenCategory
  case object KW_PROTECTED extends TokenCategory
  case object KW_PRIVATE extends TokenCategory
  case object KW_INTERNAL extends TokenCategory
  case object KW_ABSTRACT extends TokenCategory
  case object KW_FINAL extends TokenCategory
  case object KW_OVERRIDE extends TokenCategory
  case object KW_VIRTUAL extends TokenCategory
  case object KW_IF extends TokenCategory
  case object KW_THEN extends TokenCategory
  case object KW_ELSE extends TokenCategory
  case object KW_ELSIF extends TokenCategory
  case object KW_MATCH extends TokenCategory
  case object KW_CASE extends TokenCategory
  case object KW_AND extends TokenCategory
  case object KW_OR extends TokenCategory
  case object KW_TYPE extends TokenCategory
  case object KW_BIGTYPE extends TokenCategory
  case object KW_UNDERSCORE extends TokenCategory
  case object KW_EXTERN extends TokenCategory
  case object KW_RAISE extends TokenCategory
  case object KW_BEGIN extends TokenCategory
  case object KW_RESCUE extends TokenCategory
  case object KW_FINALLY extends TokenCategory
  case object KW_ERASED extends TokenCategory
  case object KW_TOKEN extends TokenCategory
  case object KW_REQUIRES extends TokenCategory
  case object KW_ENSURES extends TokenCategory
  case object KW_MAINTAINS extends TokenCategory
  case object KW_ASSERT extends TokenCategory
  case object KW_SUMMON extends TokenCategory
  case object KW_WITNESS extends TokenCategory
  case object KW_INLINE extends TokenCategory
  case object KW_EXTENSION extends TokenCategory
  case object KW_INVERSE extends TokenCategory
  case object KW_UPDATE extends TokenCategory
  case object KW_OPERATOR extends TokenCategory
  case object KW_UNARY extends TokenCategory
  case object KW_BOXED extends TokenCategory
  case object KW_BOX extends TokenCategory
  case object KW_UNBOX extends TokenCategory
  case object KW_FN extends TokenCategory

  case object OP_LOGICAL_AND extends TokenCategory
  case object OP_LOGICAL_OR extends TokenCategory
  case object OP_EQUALS extends TokenCategory
  case object OP_NOTEQUALS extends TokenCategory
  case object OP_LESSTHANEQ extends TokenCategory
  case object OP_GREATERTHANEQ extends TokenCategory
  case object OP_SHIFTLEFT extends TokenCategory
  case object OP_SHIFTRIGHT extends TokenCategory
  case object OP_ASSIGN extends TokenCategory
  case object OP_LOGICAL_NOT extends TokenCategory
  case object OP_PLUS extends TokenCategory
  case object OP_MINUS extends TokenCategory
  case object OP_STAR extends TokenCategory
  case object OP_MUL extends TokenCategory
  case object OP_SLASH extends TokenCategory
  case object OP_DIV extends TokenCategory
  case object OP_BITAND extends TokenCategory
  case object OP_BITOR extends TokenCategory
  case object OP_BITXOR extends TokenCategory
  case object OP_BITNOT extends TokenCategory
  case object OP_LESSTHAN extends TokenCategory
  case object OP_GREATERTHAN extends TokenCategory
  case object OP_ARROW extends TokenCategory
  case object OP_FAT_ARROW extends TokenCategory
  case object OP_UNION extends TokenCategory
  case object OP_INTERSECTION extends TokenCategory
  case object OP_CONCAT extends TokenCategory
  case object OP_STARSTAR extends TokenCategory
  case object OP_PROP_EQUAL extends TokenCategory
  case object OP_PROP_DISJUNCTION extends TokenCategory
  case object OP_PROP_CONJUNCTION extends TokenCategory
  case object OP_DOTDOT extends TokenCategory
  
  
  case object SYM_DOT extends TokenCategory
  case object SYM_COMMA extends TokenCategory
  case object SYM_COLON extends TokenCategory
  case object SYM_COLONCOLON extends TokenCategory
  case object SYM_SEMICOLON extends TokenCategory
  case object SYM_OPENPAREN extends TokenCategory
  case object SYM_CLOSEPAREN extends TokenCategory
  case object SYM_OPENBRACKET extends TokenCategory
  case object SYM_CLOSEBRACKET extends TokenCategory
  case object SYM_OPENCURLY extends TokenCategory
  case object SYM_CLOSECURLY extends TokenCategory
  case object SYM_PIPE extends TokenCategory
  case object SYM_AT extends TokenCategory
  
}
