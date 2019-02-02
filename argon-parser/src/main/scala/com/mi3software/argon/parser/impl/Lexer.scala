package com.mi3software.argon.parser.impl

import com.mi3software.argon.parser._
import com.mi3software.argon.util.{FilePosition, SourceLocation, WithSource}

import scala.language.postfixOps
import scalaz._
import Scalaz._
import com.mi3software.argon.grammar.{Grammar, GrammarError, SyntaxErrorReporter}
import Grammar.Operators._
import com.mi3software.argon.parser.impl.Lexer.LexerGrammarFactory
import com.mi3software.argon.util.stream.StreamTransformation

import Function.const

object Lexer {

  private[Lexer] object Rule {
    sealed trait LexerRuleName extends Grammar.RuleLabel
    sealed trait LexerRuleNameTyped[T] extends LexerRuleName {
      override type RuleType = T
    }

    final case object NewLine extends LexerRuleNameTyped[Option[Token]]
    final case object Whitespace extends LexerRuleNameTyped[Option[Token]]
    final case object SingleQuoteString extends LexerRuleNameTyped[Option[Token]]
    final case object Integer extends LexerRuleNameTyped[Option[Token]]
    final case object Identifier extends LexerRuleNameTyped[Option[Token]]
    final case object Operator extends LexerRuleNameTyped[Option[Token]]
    final case object ResultToken extends LexerRuleNameTyped[WithSource[Option[Token]]]
  }

  private[Lexer] object LexerGrammarFactory extends Grammar.GrammarFactory[String, SyntaxError, Rule.LexerRuleName] {

    implicit val errorFactory: Grammar.ErrorFactory[String, CharacterCategory, SyntaxError] =
      new Grammar.ErrorFactory[String, CharacterCategory, SyntaxError] {
        override def createError(error: GrammarError[String, CharacterCategory]): SyntaxError =
          SyntaxError.LexerError(error)

        override def createAmbiguityError(location: SourceLocation): SyntaxError =
          SyntaxError.AmbiguousParse(location)

        override def errorEndLocationOrder: Order[SyntaxError] =
          (a, b) => implicitly[Order[FilePosition]].order(a.location.end, b.location.end)
      }

    private def token(category: CharacterCategory, s: String): TGrammar[String] = Grammar.token(category, t => t === s)
    private def tokenF(category: CharacterCategory, f: String => Boolean): TGrammar[String] = Grammar.token(category, f)
    private def partialMatcher[T](category: CharacterCategory)(f: PartialFunction[String, T]): TGrammar[T] = Grammar.matcher(category, f.lift)

    protected override def createGrammar[T](label: Rule.LexerRuleName { type RuleType = T }): TGrammar[T] =
      label match {
        case Rule.NewLine =>
          val cr = token(CharacterCategory.CR, "\r")
          val lf = token(CharacterCategory.LF, "\n")

          (lf.discard | (cr ++ lf).discard) --> const(Some(Token.NewLine))

        case Rule.Whitespace =>
          (tokenF(CharacterCategory.Whitespace, s => Character.isWhitespace(s.codePointAt(0)) && s != "\r" && s != "\n")+~) --> const(None)

        case Rule.SingleQuoteString =>
          val singleQuote = token(CharacterCategory.SingleQuote, "'")

          val anyChar = tokenF(CharacterCategory.SingleQuoteStringChar, _ =/= "'")

          (singleQuote ++ ((
            singleQuote ++ singleQuote --> const("'") |
              anyChar
            )*) ++ singleQuote) --> {
            case (_, chs, _) =>
              Some(Token.StringToken(NonEmptyList(
                Token.StringToken.StringPart(chs.mkString)
              )))
          }

        case Rule.Integer =>
          val digit = partialMatcher[BigInt](CharacterCategory.NumberDigit) {
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

          val decDigit = partialMatcher[BigInt](CharacterCategory.NonZeroDigit) {
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

          val numBase = partialMatcher[BigInt](CharacterCategory.BaseSpecifier) {
            case "X" | "x" => 16
            case "B" | "b" => 2
            case "o" => 8
          }

          val withBaseSpec =
            (token(CharacterCategory.Zero, "0") ++ numBase ++ (digit*)) --> {
              case (_, base, content) => Some(Token.IntToken(1, base, content)) : Option[Token]
            }

          val decimalNum =
            decDigit ++ decDigit ++ (digit*) --> { case (d1, d2, tail) => Some(Token.IntToken(1, 10, Vector(d1, d2) ++ tail)) : Option[Token] }

          val singleDigit = decDigit --> { d => Some(Token.IntToken(1, 10, Vector(d))) : Option[Token] }

          withBaseSpec | decimalNum | singleDigit

        case Rule.Identifier =>
          def startChar = tokenF(CharacterCategory.Letter, ch => Character.isLetter(ch.codePointAt(0))) | token(CharacterCategory.Underscore, "_")
          def idChar = startChar | tokenF(CharacterCategory.Digit, ch => Character.isDigit(ch.codePointAt(0)))
          def idTerminator = token(CharacterCategory.QMark, "?") | token(CharacterCategory.Exclaim, "!")

          def createToken(id: String): Token = id match {
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
            case "_" => Token.KW_UNDERSCORE

            case _ => Token.Identifier(id);
          }


          (startChar ++ (idChar*) ++ (idTerminator?)) --> {
            case (start, inner, term) =>
              Some(createToken(start + inner.mkString + term.toList.mkString))
          }

        case Rule.Operator =>

          def op(grammar: TGrammar[_], t: Token): TGrammar[Option[Token]] =
            grammar --> const(Some(t))

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


          op(and ++ and, Token.OP_BOOLAND) |
            op(or ++ or, Token.OP_BOOLOR) |
            op(exclaim ++ equal, Token.OP_NOTEQUALS) |
            op(lessThan ++ equal, Token.OP_LESSTHANEQ) |
            op(greaterThan ++ equal, Token.OP_GREATERTHANEQ) |
            op(lessThan ++ lessThan, Token.OP_SHIFTLEFT) |
            op(greaterThan ++ greaterThan, Token.OP_SHIFTRIGHT) |
            op(lessThan ++ colon, Token.OP_SUBTYPE) |
            op(greaterThan ++ colon, Token.OP_SUPERTYPE) |
            op(colon ++ equal, Token.OP_ASSIGN) |
            op(minus ++ greaterThan, Token.OP_LAMBDA_TYPE) |
            op(equal ++ greaterThan, Token.OP_LAMBDA) |
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
            op(star, Token.OP_MUL) |
            op(token(CharacterCategory.Times, "×"), Token.OP_MUL) |
            op(slash, Token.OP_DIV) |
            op(token(CharacterCategory.Divide, "÷"), Token.OP_DIV) |
            op(and, Token.OP_BITAND) |
            op(or, Token.OP_BITOR) |
            op(caret, Token.OP_BITXOR) |
            op(tilde, Token.OP_BITNOT) |
            op(lessThan, Token.OP_LESSTHAN) |
            op(greaterThan, Token.OP_GREATERTHAN) |
            op(colon, Token.OP_COLON)

        case Rule.ResultToken => (
          rule(Rule.NewLine) |
            rule(Rule.Whitespace) |
            rule(Rule.SingleQuoteString) |
            rule(Rule.Integer) |
            rule(Rule.Identifier) |
            rule(Rule.Operator)
        ).observeSource
      }
  }

  type ErrorEffect[F[_], A] = EitherT[F, NonEmptyList[SyntaxError], A]


  type ErrorReporter[F[_]] = SyntaxErrorReporter[F, SyntaxError]

  def lex[F[_]: ErrorReporter]: StreamTransformation[F, WithSource[String], FilePosition, WithSource[Token], FilePosition] =
    Grammar.parseAll(LexerGrammarFactory)(Rule.ResultToken)
      .flatMapItems {
        case WithSource(Some(value), loc) => Vector(WithSource(value, loc))
        case _ => Vector.empty
      }


}
