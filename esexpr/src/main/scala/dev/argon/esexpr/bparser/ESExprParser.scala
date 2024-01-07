package dev.argon.esexpr.bparser

import dev.argon.esexpr.{ESExpr, ESExprException}
import dev.argon.grammar.Grammar.ErrorFactory
import dev.argon.grammar.Grammar.Operators.*
import dev.argon.grammar.{Grammar, GrammarError}
import dev.argon.util.{*, given}
import zio.*
import zio.stream.*

import scala.Function.const
import scala.reflect.TypeTest

object ESExprParser {

  final case class ParseError(val fileName: Option[String], val error: GrammarError[ESExprToken, Unit, FileOffset]) extends ESExprException(s"Could not parse ESExpr: ${error}")

  private[ESExprParser] object Rule {
    sealed trait RuleName[T]

    object RuleName {
      given[T, U]: CanEqual[RuleName[T], RuleName[U]] = CanEqual.canEqualAny
    }

    case object Expr extends RuleName[ESExprSP]
    case object Constructed extends RuleName[ESExprSP]
    case object Float32 extends RuleName[ESExprSP]
    case object Float64 extends RuleName[ESExprSP]
    case object Int extends RuleName[ESExprSP]
    case object Str extends RuleName[ESExprSP]
    case object Bool extends RuleName[ESExprSP]
    case object Null extends RuleName[ESExprSP]
  }

  private[ESExprParser] final class ParserGrammarFactory(override val fileName: Option[String]) extends Grammar.GrammarFactory[ESExprToken, FileOffset, ParseError, Rule.RuleName] {

    private given ErrorFactory[ESExprToken, Unit, ParseError, FileOffset] with
      override def createError(error: GrammarError[ESExprToken, Unit, FileOffset]): ParseError =
        ParseError(fileName, error)

      override def errorEndLocationOrder: Ordering[ParseError] = new Ordering[ParseError] {
        override def compare(x: ParseError, y: ParseError): Int =
          x.error.location.end.compareTo(y.error.location.end)
      }
    end given

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
            (token[ESExprToken.KeywordArgument] ++! rule(Rule.Expr)) --> { (name, value) => (Some(name.nameIndex), value) } |
              rule(Rule.Expr) --> { value => (None, value) }

          token[ESExprToken.ConstructorStart] ++ parameter.* ++ token[ESExprToken.ConstructorEnd.type] --> {
              case (ESExprToken.ConstructorStart(name), params, _) =>
                params.foldLeft[ESExprSP.Constructed](ESExprSP.Constructed(name, Map.empty, Seq.empty)) {
                  case (prev, (Some(name), value)) =>
                    prev.copy(kwargs = prev.kwargs + (name -> value))

                  case (prev, (None, value)) =>
                    prev.copy(args = prev.args :+ value)
                }
          }

        case Rule.Float32 =>
          token[ESExprToken.Float32Value] --> { t => ESExprSP.Float32(t.value) }

        case Rule.Float64 =>
          token[ESExprToken.Float64Value] --> { t => ESExprSP.Float64(t.value) }

        case Rule.Int =>
          token[ESExprToken.IntValue] --> { t => ESExprSP.Int(t.value) }

        case Rule.Str =>
          token[ESExprToken.StringValue] --> { t => ESExprSP.Str(t.value) } |
            token[ESExprToken.StringPoolValue] --> { t => ESExprSP.StrPooled(t.index) }

        case Rule.Bool =>
          token[ESExprToken.BooleanValue] --> { t => ESExprSP.Bool(t.value) }

        case Rule.Null =>
          token[ESExprToken.NullValue.type] --> { _ => ESExprSP.Null }
      }
  }

  def parse(fileName: Option[String])
  : ZChannel[Any, Nothing, Chunk[WithLocation[ESExprToken, FileOffset]], FileOffset, ParseError, Chunk[ESExprSP], FileOffset] =
    Grammar.parseAll(ParserGrammarFactory(fileName))(Rule.Expr)
}
