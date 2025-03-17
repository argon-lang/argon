package dev.argon.parser

import dev.argon.parser.*
import dev.argon.util.*

import scala.language.postfixOps
import dev.argon.grammar.{Grammar, GrammarError}
import Grammar.Operators.*
import dev.argon.ast.StringFragment
import zio.Chunk
import zio.stream.ZChannel
import cats.data.NonEmptySeq

import Function.const

object Lexer {

  private[Lexer] object Rule {
    sealed trait LexerRuleName[T]

    object LexerRuleName {
      given [T, U] => CanEqual[LexerRuleName[T], LexerRuleName[U]] = CanEqual.canEqualAny
    }

    case object NewLine extends LexerRuleName[Token]
    case object Whitespace extends LexerRuleName[Unit]
    case object DoubleQuoteString extends LexerRuleName[Token]
    case object HexDigit extends LexerRuleName[BigInt]
    case object Integer extends LexerRuleName[Token]
    case object Identifier extends LexerRuleName[Token]
    case object Operator extends LexerRuleName[Token]
    case object ResultToken extends LexerRuleName[Option[WithSource[Token]]]
    case object NonEmptyToken extends LexerRuleName[Token]
  }

  private[Lexer] final class LexerGrammarFactory(override val fileName: Option[String]) extends Grammar.GrammarFactory[String, CharacterCategory, FilePosition, Rule.LexerRuleName] {

    private def token(category: CharacterCategory, s: String): TGrammar[String] = Grammar.token(category, t => t == s)
    private def tokenF(category: CharacterCategory, f: String => Boolean): TGrammar[String] = Grammar.token(category, f)

    private def partialMatcher[T](category: CharacterCategory)(f: PartialFunction[String, T]): TGrammar[T] =
      Grammar.matcher(category, f.lift)

    protected override def createGrammar[T](label: Rule.LexerRuleName[T]): TGrammar[T] =
      label match {
        case Rule.NewLine =>
          val cr = token(CharacterCategory.CR, "\r")
          val lf = token(CharacterCategory.LF, "\n")

          (lf.discard | (cr ++ lf).discard) --> const(Token.NewLine)

        case Rule.Whitespace =>
          (tokenF(
            CharacterCategory.Whitespace,
            s => Character.isWhitespace(s.codePointAt(0)) && s != "\r" && s != "\n",
          ) +~) --> const(())

        case Rule.DoubleQuoteString =>
          val doubleQuote = token(CharacterCategory.Quote, "\"")
          val esc = token(CharacterCategory.StringEscape, "\\").discard

          def singleEscape(ch: String, value: String): TGrammar[StringFragment] =
            esc ++! token(CharacterCategory.StringEscape, ch) --> const(StringFragment.Text(value))

          def isValidStringChar(c: String): Boolean = c != "\"" && c != "\\" && c != "#"
          val anyChar =
            tokenF(CharacterCategory.StringChar, isValidStringChar) --> StringFragment.Text.apply

          val unicodeEscape: TGrammar[StringFragment] =
            esc ++!
              token(CharacterCategory.OpenCurly, "{").discard ++
              rule(Rule.HexDigit).+~ ++
              token(CharacterCategory.CloseCurly, "}").discard --> { digits =>
                val codepoint = digits.reduceLeft { (prev, digit) => prev * 16 + digit }

                val mask = (1L << 32) - 1
                if codepoint != (codepoint & mask) then {
                  throw new Exception("Invalid codepoint")
                }

                StringFragment.Text(new String(Character.toChars(codepoint.toInt)))
            }

          val escapeSequence =
            singleEscape("b", "\b") |
              singleEscape("f", "\f") |
              singleEscape("n", "\n") |
              singleEscape("r", "\r") |
              singleEscape("t", "\t") |
              singleEscape("v", "\u000B") |
              singleEscape("\\", "\\") |
              singleEscape("'", "'") |
              singleEscape("\"", "\"") |
              singleEscape("#", "#") |
              singleEscape("[", "[") |
              singleEscape("]", "]") |
              singleEscape("{", "{") |
              singleEscape("}", "}") |
              unicodeEscape

          (
            doubleQuote ++
              (anyChar | escapeSequence).* ++
              doubleQuote
          ).observeLocation --> {
            case WithLocation((_, parts, _), location) =>
              def combineParts(parts: Chunk[StringFragment]): Chunk[StringFragment] =
                parts match {
                  case StringFragment.Text(s1) +: StringFragment.Text(s2) +: rest =>
                    combineParts(StringFragment.Text(s1 + s2) +: rest)

                  case head +: tail =>
                    head +: combineParts(tail)

                  case _ => Chunk.empty
                }

              Token.StringToken(
                NonEmptySeq.fromSeq(combineParts(parts).toList)
                  .getOrElse { NonEmptySeq.of(StringFragment.Text("")) }
              )
          }

        case Rule.HexDigit =>
          partialMatcher[BigInt](CharacterCategory.NumberDigit) {
            case "0" => 0
            case "1" => 1
            case "2" => 2
            case "3" => 3
            case "4" => 4
            case "5" => 5
            case "6" => 6
            case "7" => 7
            case "8" => 8
            case "9" => 9
            case "A" | "a" => 0xA
            case "B" | "b" => 0xB
            case "C" | "c" => 0xC
            case "D" | "d" => 0xD
            case "E" | "e" => 0xE
            case "F" | "f" => 0xF
          }

        case Rule.Integer =>
          val digit = rule(Rule.HexDigit)

          val decDigit =
            partialMatcher[BigInt](CharacterCategory.NonZeroDigit) {
              case "0" => 0
              case "1" => 1
              case "2" => 2
              case "3" => 3
              case "4" => 4
              case "5" => 5
              case "6" => 6
              case "7" => 7
              case "8" => 8
              case "9" => 9
            }

          val numBase =
            partialMatcher[BigInt](CharacterCategory.BaseSpecifier) {
              case "X" | "x" => 16
              case "B" | "b" => 2
              case "o" => 8
            }

          val withBaseSpec =
            token(CharacterCategory.Zero, "0").discard ++ numBase ++! digit.+~ --> {
              case (base, digits) =>
                val n = digits.foldLeft(0: BigInt) { (acc, d) => acc * base + d }
                Token.IntToken(n)
            }

          val decimalNum =
            decDigit ++ decDigit.+~ --> { case (d1, tail) =>
              val digits = d1 :: tail.toList
              val n = digits.foldLeft(0: BigInt) { (acc, d) => acc * 10 + d }
              Token.IntToken(n)
            }

          val singleDigit = decDigit --> { d => Token.IntToken(d) }

          withBaseSpec | decimalNum | singleDigit

        case Rule.Identifier =>
          def startChar =
            tokenF(CharacterCategory.Letter, ch => Character.isLetter(ch.codePointAt(0))) | token(
              CharacterCategory.Underscore,
              "_",
            )
          def idChar = startChar | tokenF(CharacterCategory.Digit, ch => Character.isDigit(ch.codePointAt(0)))
          def idTerminator = token(CharacterCategory.QMark, "?") | token(CharacterCategory.Exclaim, "!")

          def createToken(id: String): Token =
            id match {
              case "__argon_builtin" => Token.KW_ARGON_BUILTIN
              case "def" => Token.KW_DEF
              case "proc" => Token.KW_PROC
              case "do" => Token.KW_DO
              case "end" => Token.KW_END
              case "var" => Token.KW_VAR
              case "val" => Token.KW_VAL
              case "record" => Token.KW_RECORD
              case "true" => Token.KW_TRUE
              case "false" => Token.KW_FALSE
              case "as" => Token.KW_AS
              case "import" => Token.KW_IMPORT
              case "export" => Token.KW_EXPORT
              case "public" => Token.KW_PUBLIC
              case "protected" => Token.KW_PROTECTED
              case "private" => Token.KW_PRIVATE
              case "internal" => Token.KW_INTERNAL
              case "if" => Token.KW_IF
              case "then" => Token.KW_THEN
              case "else" => Token.KW_ELSE
              case "elsif" => Token.KW_ELSIF
              case "type" => Token.KW_TYPE
              case "type!" => Token.KW_BIGTYPE
              case "extern" => Token.KW_EXTERN
              case "raise" => Token.KW_RAISE
              case "begin" => Token.KW_BEGIN
              case "rescue" => Token.KW_RESCUE
              case "finally" => Token.KW_FINALLY
              case "erased" => Token.KW_ERASED
              case "requires" => Token.KW_REQUIRES
              case "ensures" => Token.KW_ENSURES
              case "maintains" => Token.KW_MAINTAINS
              case "assert" => Token.KW_ASSERT
              case "summon" => Token.KW_SUMMON
              case "proof" => Token.KW_PROOF
              case "extension" => Token.KW_EXTENSION
              case "inverse" => Token.KW_INVERSE
              case "update" => Token.KW_UPDATE
              case "inline" => Token.KW_INLINE
              case "operator" => Token.KW_OPERATOR
              case "unary" => Token.KW_UNARY
              case "boxed" => Token.KW_BOXED
              case "box" => Token.KW_BOX
              case "unbox" => Token.KW_UNBOX
              case "_" => Token.KW_UNDERSCORE

              case _ => Token.Identifier(id)
            }

          (startChar ++ (idChar*) ++ (idTerminator ?)) --> {
            case (start, inner, term) =>
              createToken(start + inner.mkString + term.toList.mkString)
          }

        case Rule.Operator =>
          def op(grammar: TGrammar[?], t: Token): TGrammar[Token] = grammar --> const(t)

          val and = token(CharacterCategory.And, "&")
          val or = token(CharacterCategory.Or, "|")
          val exclaim = token(CharacterCategory.Exclaim, "!")
          val equal = token(CharacterCategory.Equals, "=")
          val lessThan = token(CharacterCategory.LessThan, "<")
          val greaterThan = token(CharacterCategory.GreaterThan, ">")
          val colon = token(CharacterCategory.Colon, ":")
          val plus = token(CharacterCategory.Plus, "+")
          val minus = token(CharacterCategory.Minus, "-")
          val dot = token(CharacterCategory.Dot, ".")
          val comma = token(CharacterCategory.Comma, ",")
          val semicolon = token(CharacterCategory.Semicolon, ";")
          val openParen = token(CharacterCategory.OpenParen, "(")
          val closeParen = token(CharacterCategory.CloseParen, ")")
          val openSquare = token(CharacterCategory.OpenSquare, "[")
          val closeSquare = token(CharacterCategory.CloseSquare, "]")
          val openCurly = token(CharacterCategory.OpenCurly, "{")
          val closeCurly = token(CharacterCategory.CloseCurly, "}")
          val star = token(CharacterCategory.Star, "*")
          val slash = token(CharacterCategory.Slash, "/")
          val backslash = token(CharacterCategory.Backslash, "\\")
          val caret = token(CharacterCategory.Caret, "^")
          val tilde = token(CharacterCategory.Tilde, "~")
          val atSign = token(CharacterCategory.AtSign, "@")

          op(and ++ and ++ and, Token.OP_BITAND) |
            op(or ++ or ++ or, Token.OP_BITOR) |
            op(and ++ and, Token.OP_LOGICAL_AND) |
            op(caret ++ caret ++ caret, Token.OP_BITXOR) |
            op(tilde ++ tilde ++ tilde, Token.OP_BITNOT) |
            op(lessThan ++ lessThan ++ lessThan, Token.OP_SHIFTLEFT) |
            op(greaterThan ++ greaterThan ++ greaterThan, Token.OP_SHIFTRIGHT) |
            op(or ++ or, Token.OP_LOGICAL_OR) |
            op(exclaim ++ equal, Token.OP_NOTEQUALS) |
            op(lessThan ++ equal, Token.OP_LESSTHANEQ) |
            op(greaterThan ++ equal, Token.OP_GREATERTHANEQ) |
            op(colon ++ equal, Token.OP_ASSIGN) |
            op(minus ++ greaterThan, Token.OP_LAMBDA_TYPE) |
            op(equal ++ greaterThan, Token.OP_LAMBDA) |
            op(plus ++ plus, Token.OP_CONCAT) |
            op(dot ++ dot, Token.OP_DOTDOT) |
            op(star ++ star, Token.OP_STARSTAR) |
            op(equal ++ equal, Token.OP_PROP_EQUAL) |
            op(backslash ++ slash, Token.OP_PROP_DISJUNCTION) |
            op(slash ++ backslash, Token.OP_PROP_CONJUNCTION) |
            op(equal, Token.OP_EQUALS) |
            op(token(CharacterCategory.NotEquals, "≠"), Token.OP_NOTEQUALS) |
            op(token(CharacterCategory.LessThanEq, "≤"), Token.OP_LESSTHANEQ) |
            op(token(CharacterCategory.GreaterThanEq, "≥"), Token.OP_GREATERTHANEQ) |
            op(dot, Token.OP_DOT) |
            op(comma, Token.OP_COMMA) |
            op(semicolon, Token.Semicolon) |
            op(openParen, Token.OP_OPENPAREN) |
            op(closeParen, Token.OP_CLOSEPAREN) |
            op(openSquare, Token.OP_OPENBRACKET) |
            op(closeSquare, Token.OP_CLOSEBRACKET) |
            op(openCurly, Token.OP_OPENCURLY) |
            op(closeCurly, Token.OP_CLOSECURLY) |
            op(exclaim, Token.OP_LOGICAL_NOT) |
            op(plus, Token.OP_PLUS) |
            op(minus, Token.OP_MINUS) |
            op(star, Token.OP_STAR) |
            op(token(CharacterCategory.Times, "×"), Token.OP_MUL) |
            op(slash, Token.OP_SLASH) |
            op(token(CharacterCategory.Divide, "÷"), Token.OP_DIV) |
            op(lessThan, Token.OP_LESSTHAN) |
            op(greaterThan, Token.OP_GREATERTHAN) |
            op(colon, Token.OP_COLON) |
            op(atSign, Token.OP_FUNCTION_RESULT_VALUE)

        case Rule.ResultToken =>
          (rule(Rule.NonEmptyToken).observeLocation --> Some.apply) |
            (rule(Rule.Whitespace) --> const(None: Option[WithSource[Token]]))

        case Rule.NonEmptyToken =>
          rule(Rule.NewLine) |
            rule(Rule.DoubleQuoteString) |
            rule(Rule.Integer) |
            rule(Rule.Identifier) |
            rule(Rule.Operator)

      }

  }

  def lex(fileName: Option[String])
    : ZChannel[Any, Nothing, Chunk[WithSource[String]], FilePosition, SyntaxError, Chunk[WithSource[Token]], FilePosition] =
    Grammar.parseAll(LexerGrammarFactory(fileName))(Rule.ResultToken)
      .mapOut(_.flatten)
      .mapError(SyntaxError.LexerError(fileName, _))

}
