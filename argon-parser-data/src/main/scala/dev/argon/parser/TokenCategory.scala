package dev.argon.parser

sealed trait TokenCategory derives CanEqual {
  type TokenType = this.type
}

object TokenCategory {
  final case class Wrap[T](value: TokenCategory { type TokenType = T })

  case object StringToken extends TokenCategory
  case object IntToken extends TokenCategory

  case object Identifier extends TokenCategory

  case object NewLine extends TokenCategory
  case object Semicolon extends TokenCategory

  case object KW_DEF extends TokenCategory
  case object KW_PROC extends TokenCategory
  case object KW_INSTANCE extends TokenCategory
  case object KW_CONSTRUCTOR extends TokenCategory
  case object KW_END extends TokenCategory
  case object KW_DO extends TokenCategory
  case object KW_VAR extends TokenCategory
  case object KW_VAL extends TokenCategory
  case object KW_TRUE extends TokenCategory
  case object KW_FALSE extends TokenCategory
  case object KW_AS extends TokenCategory
  case object KW_NAMESPACE extends TokenCategory
  case object KW_IMPORT extends TokenCategory
  case object KW_EXPORT extends TokenCategory
  case object KW_TRAIT extends TokenCategory
  case object KW_STATIC extends TokenCategory
  case object KW_DATA extends TokenCategory
  case object KW_PUBLIC extends TokenCategory
  case object KW_PROTECTED extends TokenCategory
  case object KW_PRIVATE extends TokenCategory
  case object KW_INTERNAL extends TokenCategory
  case object KW_BASE extends TokenCategory
  case object KW_IF extends TokenCategory
  case object KW_THEN extends TokenCategory
  case object KW_ELSE extends TokenCategory
  case object KW_ELSIF extends TokenCategory
  case object KW_OPEN extends TokenCategory
  case object KW_SEALED extends TokenCategory
  case object KW_VIRTUAL extends TokenCategory
  case object KW_ABSTRACT extends TokenCategory
  case object KW_OVERRIDE extends TokenCategory
  case object KW_FINAL extends TokenCategory
  case object KW_TYPE extends TokenCategory
  case object KW_METATYPE extends TokenCategory
  case object KW_MATCH extends TokenCategory
  case object KW_CASE extends TokenCategory
  case object KW_CLASS extends TokenCategory
  case object KW_NEW extends TokenCategory
  case object KW_FIELD extends TokenCategory
  case object KW_INITIALIZE extends TokenCategory
  case object KW_UNDERSCORE extends TokenCategory
  case object KW_GC extends TokenCategory
  case object KW_STRUCT extends TokenCategory
  case object KW_STACK extends TokenCategory
  case object KW_ANY extends TokenCategory
  case object KW_VALUETYPE extends TokenCategory
  case object KW_EXTERN extends TokenCategory
  case object KW_RAISE extends TokenCategory
  case object KW_BEGIN extends TokenCategory
  case object KW_RESCUE extends TokenCategory
  case object KW_FINALLY extends TokenCategory
  case object KW_ERASED extends TokenCategory
  case object KW_REQUIRES extends TokenCategory
  case object KW_ENSURES extends TokenCategory
  case object KW_MAINTAINS extends TokenCategory
  case object KW_ASSERT extends TokenCategory
  case object KW_SUMMON extends TokenCategory
  case object KW_PROOF extends TokenCategory
  case object KW_EXTENSION extends TokenCategory
  case object KW_INVERSE extends TokenCategory
  case object KW_UPDATE extends TokenCategory

  case object OP_BOOLAND extends TokenCategory
  case object OP_BOOLOR extends TokenCategory
  case object OP_EQUALS extends TokenCategory
  case object OP_NOTEQUALS extends TokenCategory
  case object OP_LESSTHANEQ extends TokenCategory
  case object OP_GREATERTHANEQ extends TokenCategory
  case object OP_SHIFTLEFT extends TokenCategory
  case object OP_SHIFTRIGHT extends TokenCategory
  case object OP_ASSIGN extends TokenCategory
  case object OP_DOT extends TokenCategory
  case object OP_DOTDOT extends TokenCategory
  case object OP_COMMA extends TokenCategory
  case object OP_OPENPAREN extends TokenCategory
  case object OP_CLOSEPAREN extends TokenCategory
  case object OP_OPENBRACKET extends TokenCategory
  case object OP_CLOSEBRACKET extends TokenCategory
  case object OP_OPENCURLY extends TokenCategory
  case object OP_CLOSECURLY extends TokenCategory
  case object OP_BOOLNOT extends TokenCategory
  case object OP_ADD extends TokenCategory
  case object OP_SUB extends TokenCategory
  case object OP_MUL extends TokenCategory
  case object OP_DIV extends TokenCategory
  case object OP_BITAND extends TokenCategory
  case object OP_BITOR extends TokenCategory
  case object OP_BITXOR extends TokenCategory
  case object OP_BITNOT extends TokenCategory
  case object OP_LESSTHAN extends TokenCategory
  case object OP_GREATERTHAN extends TokenCategory
  case object OP_COLON extends TokenCategory
  case object OP_SUBTYPE extends TokenCategory
  case object OP_SUPERTYPE extends TokenCategory
  case object OP_LAMBDA_TYPE extends TokenCategory
  case object OP_LAMBDA extends TokenCategory
  case object OP_UNION extends TokenCategory
  case object OP_INTERSECTION extends TokenCategory
  case object OP_CONCAT extends TokenCategory
  case object OP_STARSTAR extends TokenCategory
  case object OP_FUNCTION_RESULT_VALUE extends TokenCategory
}
