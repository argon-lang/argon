package dev.argon.esexpr.parser

import dev.argon.util.{*, given}
import dev.argon.grammar.{Grammar, GrammarError}
import Grammar.Operators.*
import dev.argon.esexpr.ESExprException
import dev.argon.grammar.Grammar.ErrorFactory
import zio.*
import zio.stream.*

import Function.const

object ESExprLexer {

  final case class LexerError(val fileName: Option[String], val error: GrammarError[String, Unit]) extends ESExprException("Could not lex ESExpr")


  private[ESExprLexer] object Rule {
    sealed trait LexerRuleName[T]
    object LexerRuleName {
      given [A, B]: CanEqual[LexerRuleName[A], LexerRuleName[B]] = CanEqual.derived
    }

    case object Whitespace extends LexerRuleName[Unit] derives CanEqual
    case object OpenParen extends LexerRuleName[ESExprToken]
    case object CloseParen extends LexerRuleName[ESExprToken]
    case object Colon extends LexerRuleName[ESExprToken]
    case object IdentifierStr extends LexerRuleName[String]
    case object Identifier extends LexerRuleName[ESExprToken]
    case object NumberLiteral extends LexerRuleName[ESExprToken]
    case object StringLiteral extends LexerRuleName[ESExprToken]
    case object Atom extends LexerRuleName[ESExprToken]
    case object ResultToken extends LexerRuleName[Option[WithSource[ESExprToken]]]
    case object NonEmptyToken extends LexerRuleName[ESExprToken]
  }

  private[ESExprLexer] final class LexerGrammarFactory(override val fileName: Option[String]) extends Grammar.GrammarFactory[String, LexerError, Rule.LexerRuleName] {

    private given ErrorFactory[String, Unit, LexerError] with
      override def createError(error: GrammarError[String, Unit]): LexerError =
        LexerError(fileName, error)

      override def errorEndLocationOrder: Ordering[LexerError] = new Ordering[LexerError] {
        override def compare(x: LexerError, y: LexerError): Int =
          x.error.location.end.compareTo(y.error.location.end)
      }
    end given

    private def token(s: String): TGrammar[String] = Grammar.token((), t => t == s)
    private def tokenF(f: String => Boolean): TGrammar[String] = Grammar.token((), f)

    private val symbols = "+-_*/%"

    private val digits = tokenF(s => Character.isDigit(s.codePointAt(0))).+~ --> { _.mkString }
    private val hexDigits = tokenF("0123456789abcdefABCDEF".contains).+~ --> { _.mkString }

    override protected def createGrammar[T](label: Rule.LexerRuleName[T]): TGrammar[T] =
      label match {
        case Rule.Whitespace =>
          tokenF(s => Character.isWhitespace(s.codePointAt(0))) --> const(())

        case Rule.OpenParen =>
          token("(") --> const(ESExprToken.OpenParen)

        case Rule.CloseParen =>
          token(")") --> const(ESExprToken.CloseParen)

        case Rule.Colon =>
          token(":") --> const(ESExprToken.Colon)

        case Rule.IdentifierStr =>
          val symbolToken = tokenF(symbols.contains)
          val letterToken = tokenF(s => Character.isLetter(s.codePointAt(0)))
          val nonStartToken = tokenF(s => Character.isLetterOrDigit(s.codePointAt(0)) || symbols.contains(s))

          symbolToken |
            (letterToken ++ nonStartToken.* --> { (start, rest) => start ++ rest.mkString })

        case Rule.Identifier =>
          rule(Rule.IdentifierStr) --> ESExprToken.Identifier.apply

        case Rule.NumberLiteral =>
          val sign = token("-") | token("+")
          val signOpt = sign.? --> { _.getOrElse("") }
          val hexIndicator = token("0") ++ token("x") --> { _ ++ _ }
          val nonZeroDigit = tokenF(s => s != "0" && Character.isDigit(s.codePointAt(0)))
          val signedInteger = signOpt ++ digits --> { (s, d) => s ++ d.mkString }
          val exponentPart = (token("e") | token("E")) ++ signedInteger --> { (e, exp) => e ++ exp.mkString }
          val floatTypeSuffix = token("f") | token("F")
          val binaryExponent = (token("p") | token("P")) ++ signedInteger --> { (p, e) => p ++ e }


          val floatingPointCommon =
            digits ++ token(".") ++ digits ++ exponentPart.? --> { (d, dot, d2, e) => d ++ dot ++ d2 ++ e.getOrElse("") } |
              hexIndicator ++ hexDigits ++ token(".") ++ hexDigits ++ binaryExponent --> { (hex, d, dot, d2, e) => hex ++ d ++ dot ++ d2 ++ e }

          val floatingPoint =
            signOpt ++ floatingPointCommon ++ floatTypeSuffix.? --> {
              case (s, n, Some(_)) => ESExprToken.Float32Literal((s ++ n).toFloat)
              case (s, n, None) => ESExprToken.Float64Literal((s ++ n).toDouble)
            }

          val integer =
            signOpt ++ token("0") --> const(ESExprToken.IntegerLiteral(0)) |
              signOpt ++ nonZeroDigit ++ digits --> { (s, d, i) => ESExprToken.IntegerLiteral(BigInt(s ++ d ++ i)) } |
              signOpt ++ hexIndicator ++ hexDigits --> { (s, _, i) => ESExprToken.IntegerLiteral(BigInt(s + i, 16)) }

          floatingPoint | integer

        case Rule.StringLiteral =>
          val quote = token("\"")
          val unescapedCh = tokenF(s => s != "\\" && s != "\"")
          val escapedCh = token("\\") ++! (
            token("f") --> const("\f") |
              token("n") --> const("\n") |
              token("r") --> const("\r") |
              token("t") --> const("\t") |
              token("\\") --> const("\\") |
              token("'") --> const("\'") |
              token("\"") --> const("\"") |
              token("u") ++ token("{") ++ hexDigits ++ token("}") --> { (_, _, digits, _) =>
                Character.toString(Integer.parseInt(digits, 16))
              }
          ) --> { (_, c) => c }

          quote ++! (unescapedCh | escapedCh).* ++ quote --> { (_, s, _) => ESExprToken.StringLiteral(s.mkString) }


        case Rule.Atom =>
          token("#") ++! rule(Rule.IdentifierStr) --> { (_, id) => ESExprToken.Atom(id) }

        case Rule.ResultToken =>
          (rule(Rule.NonEmptyToken).observeSource --> Some.apply) |
            (rule(Rule.Whitespace) --> const(Option.empty[WithSource[ESExprToken]]))

        case Rule.NonEmptyToken =>
          rule(Rule.OpenParen) |
            rule(Rule.CloseParen) |
            rule(Rule.Colon) |
            rule(Rule.Identifier) |
            rule(Rule.NumberLiteral) |
            rule(Rule.StringLiteral) |
            rule(Rule.Atom)
      }

  }

  def lex[E](fileName: Option[String])
  : ZChannel[Any, E, Chunk[WithSource[String]], FilePosition, E | LexerError, Chunk[WithSource[ESExprToken]], FilePosition] =
    Grammar.parseAll(LexerGrammarFactory(fileName))(Rule.ResultToken)
      .pipeTo(ZChannelUtil.mapElements(_.flatten))

}
