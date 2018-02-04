package com.mi3software.argon.compiler

import java.nio.charset.Charset

import com.mi3software.argon.parser._
import com.mi3software.argon.util._

sealed trait CompilationMessage {
  val file: FileSpec
  val location: SourceLocation
  def message: String

  override def toString: String =
    s"${file.name} ${location.start.line}.${location.start.position}-${location.end.line}.${location.end.line}: ${message}"
}
trait CompilationError extends CompilationMessage

object CompilationError {
  final case class SyntaxCompilerError(syntaxError: SyntaxErrorData) extends CompilationError {
    override val file: FileSpec = syntaxError.fileSpec
    override val location: SourceLocation = syntaxError.syntaxError.location

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

    private def formatToken(token: Token) = ???
    private def formatCharacter(ch: String): String = {
      val utf8 = ch.getBytes(Charset.forName("UTF-8")).map { b => f"${b & 0xFF}%02X" }.mkString(" ")

      val cp = ch.codePointAt(0)

      if(Character.isISOControl(cp) || Character.isWhitespace(cp))
        "UTF-8: " + utf8
      else
        "\"" + ch + "\" (UTF-8: " + utf8 + ")"
    }

    private def formatTokenCategory(category: TokenCategory) = ???
    private def formatCharacterCategory(category: CharacterCategory) = category match {
      case CharacterCategory.Any => "any character"
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
}
