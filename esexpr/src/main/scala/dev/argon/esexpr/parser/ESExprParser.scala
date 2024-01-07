package dev.argon.esexpr.parser

import dev.argon.esexpr.{ESExpr, ESExprException}
import dev.argon.util.{*, given}
import dev.argon.grammar.{Grammar, GrammarError}
import Grammar.Operators.*
import dev.argon.grammar.Grammar.ErrorFactory
import zio.*
import zio.stream.*

import Function.const
import scala.reflect.TypeTest

object ESExprParser {

  final case class ParseError(val fileName: Option[String], val error: GrammarError[ESExprToken, Unit, FilePosition]) extends ESExprTextParseException(s"Could not parse ESExpr: ${error}")

  private[ESExprParser] object Rule {
    sealed trait RuleName[T]

    object RuleName {
      given[T, U]: CanEqual[RuleName[T], RuleName[U]] = CanEqual.canEqualAny
    }

    case object Expr extends RuleName[ESExpr]
    case object Constructed extends RuleName[ESExpr]
    case object Float32 extends RuleName[ESExpr]
    case object Float64 extends RuleName[ESExpr]
    case object Int extends RuleName[ESExpr]
    case object Str extends RuleName[ESExpr]
    case object Bool extends RuleName[ESExpr]
    case object Null extends RuleName[ESExpr]

    case object Result extends RuleName[WithSource[ESExpr]]
  }

  private[ESExprParser] final class ParserGrammarFactory(override val fileName: Option[String]) extends Grammar.GrammarFactory[ESExprToken, FilePosition, ParseError, Rule.RuleName] {

    private given ErrorFactory[ESExprToken, Unit, ParseError, FilePosition] with
      override def createError(error: GrammarError[ESExprToken, Unit, FilePosition]): ParseError =
        ParseError(fileName, error)

      override def errorEndLocationOrder: Ordering[ParseError] = new Ordering[ParseError] {
        override def compare(x: ParseError, y: ParseError): Int =
          x.error.location.end.compareTo(y.error.location.end)
      }
    end given

    private def token(t: ESExprToken): TGrammar[ESExprToken] = Grammar.token((), _ == t)
    private def token[T <: ESExprToken](using TypeTest[ESExprToken, T]): TGrammar[T] =
      Grammar.partialMatcher(()) {
        case t: T => t
      }


    override protected def createGrammar[T](label: Rule.RuleName[T]): TGrammar[T] =
      label match {
        case Rule.Expr =>
          rule(Rule.Constructed) |
            rule(Rule.Float32) |
            rule(Rule.Float64) |
            rule(Rule.Int) |
            rule(Rule.Str) |
            rule(Rule.Bool) |
            rule(Rule.Null)

        case Rule.Constructed =>
          val parameter =
            (token[ESExprToken.Identifier] ++! token(ESExprToken.Colon) ++ rule(Rule.Expr)) --> { (name, _, value) => (Some(name.name), value) } |
              rule(Rule.Expr) --> { value => (None, value) }

          token(ESExprToken.OpenParen) ++! token[ESExprToken.Identifier] ++ parameter.* ++ token(ESExprToken.CloseParen) --> {
              case (_, ESExprToken.Identifier(name), params, _) =>
                params.foldLeft[ESExpr.Constructed](ESExpr.Constructed(name, Map.empty, Seq.empty)) {
                  case (prev, (Some(name), value)) =>
                    prev.copy(kwargs = prev.kwargs + (name -> value))

                  case (prev, (None, value)) =>
                    prev.copy(args = prev.args :+ value)
                }
          }

        case Rule.Float32 =>
          token[ESExprToken.Float32Literal] --> { t => ESExpr.Float32(t.f) }

        case Rule.Float64 =>
          token[ESExprToken.Float64Literal] --> { t => ESExpr.Float64(t.d) }

        case Rule.Int =>
          token[ESExprToken.IntegerLiteral] --> { t => ESExpr.Int(t.i) }

        case Rule.Str =>
          token[ESExprToken.StringLiteral] --> { t => ESExpr.Str(t.s) }

        case Rule.Bool =>
          token(ESExprToken.Atom("true")) --> const(ESExpr.Bool(true)) |
            token(ESExprToken.Atom("false")) --> const(ESExpr.Bool(false))

        case Rule.Null =>
          token(ESExprToken.Atom("null")) --> const(ESExpr.Null)

        case Rule.Result =>
          rule(Rule.Expr).observeLocation
      }
  }

  def parse(fileName: Option[String])
  : ZChannel[Any, Nothing, Chunk[WithSource[ESExprToken]], FilePosition, ParseError, Chunk[WithSource[ESExpr]], FilePosition] =
    Grammar.parseAll(ParserGrammarFactory(fileName))(Rule.Result)
}
