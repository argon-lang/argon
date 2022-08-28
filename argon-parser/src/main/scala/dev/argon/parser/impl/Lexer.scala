package dev.argon.parser.impl

import dev.argon.parser.*
import dev.argon.util.*
import scala.language.postfixOps
import dev.argon.grammar.{Grammar, GrammarError}
import Grammar.Operators.*
import zio.Chunk
import zio.stream.ZChannel
import Function.const

object Lexer {

  private[Lexer] object Rule {
    sealed trait LexerRuleName[T]

    object LexerRuleName {
      given [T, U]: CanEqual[LexerRuleName[T], LexerRuleName[U]] = CanEqual.canEqualAny
    }

    case object NewLine extends LexerRuleName[Token]
    case object Whitespace extends LexerRuleName[Unit]
    case object DoubleQuoteString extends LexerRuleName[Token]
    case object SingleQuoteString extends LexerRuleName[Token]
    case object HexDigit extends LexerRuleName[BigInt]
    case object Integer extends LexerRuleName[Token]
    case object Identifier extends LexerRuleName[Token]
    case object Operator extends LexerRuleName[Token]
    case object ResultToken extends LexerRuleName[Option[WithSource[Token]]]
    case object NonEmptyToken extends LexerRuleName[Token]
  }

  private[Lexer] final class LexerGrammarFactory(override val fileName: Option[String]) extends Grammar.GrammarFactory[String, SyntaxError, Rule.LexerRuleName] {

    implicit val errorFactory: Grammar.ErrorFactory[String, CharacterCategory, SyntaxError] =
      new Grammar.ErrorFactory[String, CharacterCategory, SyntaxError] {

        override def createError(error: GrammarError[String, CharacterCategory]): SyntaxError =
          SyntaxError.LexerError(fileName, error)

        override def errorEndLocationOrder: Ordering[SyntaxError] =
          (a, b) => implicitly[Ordering[FilePosition]].compare(a.location.end, b.location.end)

      }

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
          val esc = token(CharacterCategory.StringEscape, "\\")

          def singleEscape(ch: String, value: String): TGrammar[Token.StringToken.StringPart] =
            (esc ++! token(CharacterCategory.StringEscape, ch)).observeSource --> {
              case WithSource(_, location) =>
                Token.StringToken.StringPart(WithSource(value, location))
            }

          def isValidStringChar(c: String): Boolean = c != "\"" && c != "\\" && c != "#"
          val anyChar =
            tokenF(CharacterCategory.StringChar, isValidStringChar).observeSource --> Token.StringToken.StringPart.apply

          val unicodeEscape: TGrammar[Token.StringToken.StringPart] =
            (esc ++!
              token(CharacterCategory.OpenCurly, "{") ++
              rule(Rule.HexDigit).+~ ++
              token(CharacterCategory.CloseCurly, "}")).observeSource --> {
              case WithSource((_, _, digits, _), location) =>
                val codepoint = digits.reduceLeft { (prev, digit) => prev * 16 + digit }

                val mask = (1L << 32) - 1
                if codepoint != (codepoint & mask) then {
                  throw new Exception("Invalid codepoint")
                }

                Token.StringToken.StringPart(WithSource(new String(Character.toChars(codepoint.toInt)), location))
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

          val escapeSequenceStr = escapeSequence --> { _.str }

          val interpolation: TGrammar[Token.StringToken.ExprPart] = {

            val interpStart = token(CharacterCategory.Hash, "#")

            val formatStart = token(CharacterCategory.OpenSquare, "[")
            val formatEnd = token(CharacterCategory.CloseSquare, "]")
            val formatChar = tokenF(CharacterCategory.StringChar, c => isValidStringChar(c) && c != "]")

            val subExprStart = token(CharacterCategory.OpenCurly, "{")

            val formatStr =
              formatStart ++ (formatChar | escapeSequenceStr).* ++ formatEnd --> {
                case (_, parts, _) =>
                  parts.mkString
              }

            val innerExprGrammar =
              new Grammar.EmbeddedGrammar[
                SyntaxError,
                String,
                Rule.LexerRuleName,
                Token,
                ArgonParser.Rule.ArgonRuleName,
                Expr,
              ] {
                protected override val outerGrammar: Grammar[String, SyntaxError, Rule.LexerRuleName, Token] =
                  rule(Rule.Whitespace).* ++ rule(Rule.NonEmptyToken) --> { case (_, token) => token }

                protected override val innerGrammar: Grammar[Token, SyntaxError, ArgonParser.Rule.ArgonRuleName, Expr] =
                  ArgonParser.ArgonGrammarFactory(fileName).rule(ArgonParser.Rule.Expression)

                protected override val innerFactory
                  : Grammar.GrammarFactory[Token, SyntaxError, ArgonParser.Rule.ArgonRuleName] =
                  ArgonParser.ArgonGrammarFactory(fileName)

                override def stopToken(token: Token): Boolean =
                  token match {
                    case Token.OP_CLOSECURLY => true
                    case _ => false
                  }

                override def unexpectedEndOfFileError(pos: FilePosition): SyntaxError =
                  SyntaxError.LexerError(fileName, GrammarError.UnexpectedEndOfFile(CharacterCategory.CloseCurly, fileName, pos))

                override def unexpectedToken(token: WithSource[Token]): SyntaxError =
                  SyntaxError.ParserError(fileName, GrammarError.UnexpectedToken(TokenCategory.OP_CLOSECURLY, token))
              }

            interpStart ++! (formatStr.observeSource.? ++ subExprStart ++ innerExprGrammar.observeSource) --> {
              case (_, (format, _, expr)) =>
                Token.StringToken.ExprPart(format, expr)
            }
          }

          (
            doubleQuote ++
              (anyChar | escapeSequence | interpolation).* ++
              doubleQuote
          ).observeSource --> {
            case WithSource((_, parts, _), location) =>
              def combineParts(parts: Chunk[Token.StringToken.Part]): Chunk[Token.StringToken.Part] =
                parts match {
                  case Token.StringToken.StringPart(
                        WithSource(s1, loc1)
                      ) +: Token.StringToken.StringPart(WithSource(s2, loc2)) +: rest =>
                    combineParts(Token.StringToken.StringPart(WithSource(s1 + s2, SourceLocation.merge(loc1, loc2))) +: rest)

                  case head +: tail =>
                    head +: combineParts(tail)

                  case _ => Chunk.empty
                }

              Token.StringToken(
                NonEmptyList.fromList(combineParts(parts).toList)
                  .getOrElse { NonEmptyList(Token.StringToken.StringPart(WithSource("", location))) }
              )
          }

        case Rule.SingleQuoteString =>
          val singleQuote = token(CharacterCategory.Quote, "'")

          val anyChar = tokenF(CharacterCategory.StringChar, _ != "'")

          (singleQuote ++ ((
            singleQuote ++ singleQuote --> const("'") |
              anyChar
          ).*).observeSource ++ singleQuote) --> {
            case (_, chs, _) =>
              Token.StringToken(NonEmptyList(
                Token.StringToken.StringPart(chs.map(_.mkString))
              ))
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
            (token(CharacterCategory.Zero, "0") ++ numBase ++ (digit*)) --> {
              case (_, base, content) => Token.IntToken(1, base, content.toVector)
            }

          val decimalNum =
            decDigit ++ decDigit ++ (digit*) --> { case (d1, d2, tail) =>
              Token.IntToken(1, 10, Vector(d1, d2) ++ tail)
            }

          val singleDigit = decDigit --> { d => Token.IntToken(1, 10, Vector(d)) }

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
              case "def" => Token.KW_DEF
              case "proc" => Token.KW_PROC
              case "instance" => Token.KW_INSTANCE
              case "constructor" => Token.KW_CONSTRUCTOR
              case "do" => Token.KW_DO
              case "end" => Token.KW_END
              case "var" => Token.KW_VAR
              case "val" => Token.KW_VAL
              case "true" => Token.KW_TRUE
              case "false" => Token.KW_FALSE
              case "as" => Token.KW_AS
              case "namespace" => Token.KW_NAMESPACE
              case "import" => Token.KW_IMPORT
              case "export" => Token.KW_EXPORT
              case "trait" => Token.KW_TRAIT
              case "static" => Token.KW_STATIC
              case "data" => Token.KW_DATA
              case "public" => Token.KW_PUBLIC
              case "protected" => Token.KW_PROTECTED
              case "private" => Token.KW_PRIVATE
              case "internal" => Token.KW_INTERNAL
              case "base" => Token.KW_BASE
              case "if" => Token.KW_IF
              case "then" => Token.KW_THEN
              case "else" => Token.KW_ELSE
              case "elsif" => Token.KW_ELSIF
              case "open" => Token.KW_OPEN
              case "sealed" => Token.KW_SEALED
              case "virtual" => Token.KW_VIRTUAL
              case "abstract" => Token.KW_ABSTRACT
              case "override" => Token.KW_OVERRIDE
              case "final" => Token.KW_FINAL
              case "type" => Token.KW_TYPE
              case "match" => Token.KW_MATCH
              case "case" => Token.KW_CASE
              case "class" => Token.KW_CLASS
              case "new" => Token.KW_NEW
              case "field" => Token.KW_FIELD
              case "initialize" => Token.KW_INITIALIZE
              case "extern" => Token.KW_EXTERN
              case "raise" => Token.KW_RAISE
              case "begin" => Token.KW_BEGIN
              case "rescue" => Token.KW_RESCUE
              case "ensure" => Token.KW_ENSURE
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
          val caret = token(CharacterCategory.Caret, "^")
          val tilde = token(CharacterCategory.Tilde, "~")

          op(and ++ and ++ and, Token.OP_BITAND) |
            op(or ++ or ++ or, Token.OP_BITOR) |
            op(and ++ and, Token.OP_BOOLAND) |
            op(caret ++ caret ++ caret, Token.OP_BITXOR) |
            op(tilde ++ tilde ++ tilde, Token.OP_BITNOT) |
            op(lessThan ++ lessThan ++ lessThan, Token.OP_SHIFTLEFT) |
            op(greaterThan ++ greaterThan ++ greaterThan, Token.OP_SHIFTRIGHT) |
            op(or ++ or, Token.OP_BOOLOR) |
            op(exclaim ++ equal, Token.OP_NOTEQUALS) |
            op(lessThan ++ equal, Token.OP_LESSTHANEQ) |
            op(greaterThan ++ equal, Token.OP_GREATERTHANEQ) |
            op(lessThan ++ colon, Token.OP_SUBTYPE) |
            op(greaterThan ++ colon, Token.OP_SUPERTYPE) |
            op(colon ++ equal, Token.OP_ASSIGN) |
            op(minus ++ greaterThan, Token.OP_LAMBDA_TYPE) |
            op(equal ++ greaterThan, Token.OP_LAMBDA) |
            op(plus ++ plus, Token.OP_CONCAT) |
            op(dot ++ dot, Token.OP_DOTDOT) |
            op(star ++ star, Token.OP_STARSTAR) |
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
            op(exclaim, Token.OP_BOOLNOT) |
            op(plus, Token.OP_ADD) |
            op(minus, Token.OP_SUB) |
            op(star, Token.OP_STAR) |
            op(token(CharacterCategory.Times, "×"), Token.OP_MUL) |
            op(slash, Token.OP_SLASH) |
            op(token(CharacterCategory.Divide, "÷"), Token.OP_DIV) |
            op(and, Token.OP_INTERSECTION) |
            op(or, Token.OP_UNION) |
            op(lessThan, Token.OP_LESSTHAN) |
            op(greaterThan, Token.OP_GREATERTHAN) |
            op(colon, Token.OP_COLON)

        case Rule.ResultToken =>
          (rule(Rule.NonEmptyToken).observeSource --> Some.apply) |
            (rule(Rule.Whitespace) --> const(None: Option[WithSource[Token]]))

        case Rule.NonEmptyToken =>
          rule(Rule.NewLine) |
            rule(Rule.SingleQuoteString) |
            rule(Rule.DoubleQuoteString) |
            rule(Rule.Integer) |
            rule(Rule.Identifier) |
            rule(Rule.Operator)

      }

  }

  def lex[E](fileName: Option[String])
    : ZChannel[Any, E, Chunk[WithSource[String]], FilePosition, E | SyntaxError, Chunk[WithSource[Token]], FilePosition] =
    Grammar.parseAll(LexerGrammarFactory(fileName))(Rule.ResultToken)
      .pipeTo(ZChannelUtil.mapElements(_.flatten))

}
