package com.mi3software.argon.compiler

import java.nio.charset.Charset

import com.mi3software.argon.parser._
import com.mi3software.argon.util._

import com.mi3software.argon.module.ArgonModule

sealed trait CompilationMessage {
  val source: CompilationMessageSource
  def message: String

  override def toString: String =
    s"${source.formatted}: ${message}"
}
trait CompilationError extends CompilationMessage

object CompilationError {
  final case class SyntaxCompilerError(syntaxError: SyntaxErrorData) extends CompilationError {
    override val source: CompilationMessageSource = CompilationMessageSource.SourceFile(syntaxError.fileSpec, syntaxError.syntaxError.location)

    override def message: String = syntaxError.syntaxError match {
      case SyntaxError.InvalidSurrogatePairs(ch, _) => s"Invalid surrogate: ${ch.toInt.toHexString}"
      case SyntaxError.UnexpectedCombingCharacter(cp, _) => s"Unexpected combing character: ${(cp.toLong & 0xFFFFFFFF).toHexString}"
      case SyntaxError.LexerError(error) => convertGrammarError("character", formatCharacter, formatCharacterCategory, error)
      case SyntaxError.ParserError(error) => convertGrammarError("token", formatToken, formatTokenCategory, error)
      case SyntaxError.AmbiguousParse(_) => "Parse was ambiguous"
    }

    private def convertGrammarError[TToken, TTokenCategory]
    (
      tokenName: String,
      tokenFormatter: TToken => String,
      categoryFormatter: TTokenCategory => String,
      grammarError: GrammarError[TToken, TTokenCategory]
    ): String =
      grammarError match {
        case GrammarError.InfiniteRecursion(_) => "The parser detected an infinitely recursive rule."
        case GrammarError.ExpectedEndOfFile(WithSource(token, _)) => s"Expected end of file, but found ${tokenFormatter(token)}"
        case GrammarError.UnexpectedEndOfFile(tokenCategory, _) => s"Expected ${categoryFormatter(tokenCategory)}, but found end of file"
        case GrammarError.UnexpectedToken(expectedCategory, WithSource(token, _)) => s"Expected ${categoryFormatter(expectedCategory)}, but found ${tokenFormatter(token)}"
      }

    private def formatToken(token: Token) =
      formatTokenCategory(token.category)

    private def formatCharacter(ch: String): String = {
      val utf8 = ch.getBytes(Charset.forName("UTF-8")).map { b => f"${b & 0xFF}%02X" }.mkString(" ")

      val cp = ch.codePointAt(0)

      if(Character.isISOControl(cp) || Character.isWhitespace(cp))
        "UTF-8: " + utf8
      else
        "\"" + ch + "\" (UTF-8: " + utf8 + ")"
    }

    private def formatTokenCategory(category: TokenCategory) = category match {
      case TokenCategory.StringToken => "string literal"
      case TokenCategory.IntToken => "int literal"

      case TokenCategory.Identifier => "identifier"


      case TokenCategory.NewLine => "new line"
      case TokenCategory.Semicolon => "semicolon"

      case TokenCategory.KW_DEF => "def"
      case TokenCategory.KW_PROC => "proc"
      case TokenCategory.KW_INSTANCE => "instance"
      case TokenCategory.KW_CONSTRUCTOR => "constructor"
      case TokenCategory.KW_END => "end"
      case TokenCategory.KW_DO => "do"
      case TokenCategory.KW_VAR => "var"
      case TokenCategory.KW_VAL => "val"
      case TokenCategory.KW_TRUE => "true"
      case TokenCategory.KW_FALSE => "false"
      case TokenCategory.KW_AS => "as"
      case TokenCategory.KW_NAMESPACE => "namespace"
      case TokenCategory.KW_IMPORT => "import"
      case TokenCategory.KW_TRAIT => "trait"
      case TokenCategory.KW_STATIC => "static"
      case TokenCategory.KW_DATA => "data"
      case TokenCategory.KW_PUBLIC => "public"
      case TokenCategory.KW_PROTECTED => "protected"
      case TokenCategory.KW_PRIVATE => "private"
      case TokenCategory.KW_INTERNAL => "internal"
      case TokenCategory.KW_BASE => "base"
      case TokenCategory.KW_IF => "if"
      case TokenCategory.KW_THEN => "then"
      case TokenCategory.KW_ELSE => "else"
      case TokenCategory.KW_ELSIF => "elsif"
      case TokenCategory.KW_OPEN => "open"
      case TokenCategory.KW_SEALED => "sealed"
      case TokenCategory.KW_VIRTUAL => "virtual"
      case TokenCategory.KW_ABSTRACT => "abstract"
      case TokenCategory.KW_OVERRIDE => "override"
      case TokenCategory.KW_FINAL => "final"
      case TokenCategory.KW_TYPE => "type"
      case TokenCategory.KW_MATCH => "match"
      case TokenCategory.KW_CASE => "case"
      case TokenCategory.KW_CLASS => "class"
      case TokenCategory.KW_NEW => "new"
      case TokenCategory.KW_FIELD => "field"
      case TokenCategory.KW_INITIALIZE => "initialize"
      case TokenCategory.KW_UNDERSCORE => "_"
      case TokenCategory.KW_GC => "gc"
      case TokenCategory.KW_STRUCT => "struct"
      case TokenCategory.KW_STACK => "stack"
      case TokenCategory.KW_ANY => "any"
      case TokenCategory.KW_VALUETYPE => "valuetype"

      case TokenCategory.OP_BOOLAND => "&&"
      case TokenCategory.OP_BOOLOR => "||"
      case TokenCategory.OP_EQUALS => "="
      case TokenCategory.OP_NOTEQUALS => "≠ (or !=)"
      case TokenCategory.OP_LESSTHANEQ => "≤ (or <=)"
      case TokenCategory.OP_GREATERTHANEQ => "≥ (or >=)"
      case TokenCategory.OP_SHIFTLEFT => "<<"
      case TokenCategory.OP_SHIFTRIGHT => ">>"
      case TokenCategory.OP_ASSIGN => ":="
      case TokenCategory.OP_DOT => "."
      case TokenCategory.OP_COMMA => ","
      case TokenCategory.OP_OPENPAREN => "("
      case TokenCategory.OP_CLOSEPAREN => ")"
      case TokenCategory.OP_OPENBRACKET => "["
      case TokenCategory.OP_CLOSEBRACKET => "]"
      case TokenCategory.OP_OPENCURLY => "{"
      case TokenCategory.OP_CLOSECURLY => "}"
      case TokenCategory.OP_BOOLNOT => "!"
      case TokenCategory.OP_ADD => "+"
      case TokenCategory.OP_SUB => "-"
      case TokenCategory.OP_MUL => "× (or *)"
      case TokenCategory.OP_DIV => "÷ (or /)"
      case TokenCategory.OP_BITAND => "&"
      case TokenCategory.OP_BITOR => "|"
      case TokenCategory.OP_BITXOR => "^"
      case TokenCategory.OP_BITNOT => "~"
      case TokenCategory.OP_LESSTHAN => "<"
      case TokenCategory.OP_GREATERTHAN => ">"
      case TokenCategory.OP_COLON => ":"
      case TokenCategory.OP_SUBTYPE => "<:"
      case TokenCategory.OP_SUPERTYPE => ">:"
      case TokenCategory.OP_LAMBDA_TYPE => "->"
      case TokenCategory.OP_LAMBDA => "=>"
    }

    private def formatCharacterCategory(category: CharacterCategory) = category match {
      case CharacterCategory.SingleQuoteStringChar => "single quote string character"
      case CharacterCategory.CR => "\\r"
      case CharacterCategory.LF => "\\n"
      case CharacterCategory.Whitespace => "whitespace"
      case CharacterCategory.SingleQuote => "'"
      case CharacterCategory.NumberDigit => "digit"
      case CharacterCategory.NonZeroDigit => "non-zero digit"
      case CharacterCategory.Zero => "0"
      case CharacterCategory.Digit => "unicode digit"
      case CharacterCategory.BaseSpecifier => "base specifier"
      case CharacterCategory.Letter => "letter"
      case CharacterCategory.Underscore => "_"

      case CharacterCategory.QMark => "?"
      case CharacterCategory.Exclaim => "!"

      case CharacterCategory.And => "&"
      case CharacterCategory.Or => "|"
      case CharacterCategory.LessThan => "<"
      case CharacterCategory.GreaterThan => ">"
      case CharacterCategory.Equals => "="
      case CharacterCategory.Colon => ":"
      case CharacterCategory.Plus => "+"
      case CharacterCategory.Minus => "-"

      case CharacterCategory.NotEquals => "≠"
      case CharacterCategory.LessThanEq => "≤"
      case CharacterCategory.GreaterThanEq => "≥"

      case CharacterCategory.Dot => "."
      case CharacterCategory.Comma => ","
      case CharacterCategory.Semicolon => ";"

      case CharacterCategory.OpenParen => "("
      case CharacterCategory.CloseParen => ")"
      case CharacterCategory.OpenSquare => "["
      case CharacterCategory.CloseSquare => "]"
      case CharacterCategory.OpenCurly => "{"
      case CharacterCategory.CloseCurly => "}"

      case CharacterCategory.Star => "*"
      case CharacterCategory.Times => "×"
      case CharacterCategory.Slash => "/"
      case CharacterCategory.Divide => "÷"

      case CharacterCategory.Caret => "^"
      case CharacterCategory.Tilde => "~"
        
    }
  }

  final case class LookupFailedError(description: LookupDescription, source: CompilationMessageSource) extends CompilationError {
    override def message: String = "Could not find identifier"
  }

  final case class AmbiguousLookupError(description: LookupDescription, source: CompilationMessageSource) extends CompilationError {
    override def message: String = "Lookup is ambiguous"
  }

  final case class NamespaceUsedAsValueError(description: LookupDescription, source: CompilationMessageSource) extends CompilationError {
    override def message: String = "Namespace used as a value"
  }

  final case class UnsupportedModuleFormatVersion(version: Int, source: CompilationMessageSource) extends CompilationError {
    override def message: String = s"Unsupported module format version $version"
  }

  final case class ReferencedModuleNotFound(ref: ArgonModule.ModuleReference, source: CompilationMessageSource) extends CompilationError {
    override def message: String = s"Could not find referenced module '${ref.name}'"
  }

  final case class MissingModuleName(source: CompilationMessageSource) extends CompilationError {
    override def message: String = s"Missing module name"
  }

}

sealed trait CompilationMessageSource {
  def formatted: String
  override def toString: String = formatted
}
object CompilationMessageSource {

  final case class SourceFile(file: FileSpec, location: SourceLocation) extends CompilationMessageSource {
    override def formatted: String =
      s"${file.name} ${location.start.line}.${location.start.position}-${location.end.line}.${location.end.position}"
  }

  final case class ReferencedModule(moduleDescriptor: ModuleDescriptor) extends CompilationMessageSource {
    override def formatted: String = s"module ${moduleDescriptor.name}"
  }

}
