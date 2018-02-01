package com.mi3software.argon.parser.impl

import com.mi3software.argon.parser.impl.Token._
import com.mi3software.argon.parser._
import com.mi3software.argon.util.WithSource

import scala.reflect.ClassTag
import scala.language.postfixOps
import scalaz.{ICons, IList, INil, NonEmptyList}

import Grammar.Operators._

object Parser {

  type TGrammar[T] = Grammar[Token, TokenCategory, T]

  private def partialMatcher[T](category: TokenCategory)(f: PartialFunction[Token, T]): TGrammar[T] =
    Grammar.partialMatcher(category)(f)

  private def matchTokenFactory[TTokenCategory <: TokenCategory, TToken <: Token : ClassTag](factory: TokenWithCategory[TTokenCategory] with TokenFactory[TToken]): TGrammar[TToken] =
    partialMatcher(factory.category) { case t: TToken => t }

  private def matchToken[TToken <: TokenWithCategory[_ <: TokenCategory] with Token : ClassTag](token: TToken): TGrammar[TToken] =
    partialMatcher(token.category) { case t: TToken => t }

  private val tokenUnderscore: TGrammar[Unit] =
    matchToken(KW_UNDERSCORE).discard

  private val tokenIdentifier: TGrammar[String] =
    matchTokenFactory(Identifier) --> { _.name }



  private val ruleIdentifier: TGrammar[Option[String]] =
    tokenUnderscore --> { _ => None : Option[String] } |
      tokenIdentifier --> Some.apply


  // Expressions
  private val ruleIfExpr: TGrammar[Expr] = {

    type RuleFunc = (WithSource[Expr], WithSource[Vector[WithSource[Stmt]]]) => Expr

    lazy val ifRulePart: TGrammar[Expr] =
      (
        ruleExpressionStatement.observeSource ++ matchToken(KW_THEN) ++ ruleStatementList.observeSource ++ (
          matchToken(KW_END) -->[RuleFunc] { _ => (condition, body) => IfExpr(condition, body) } |
            (matchToken(KW_ELSE) ++ ruleStatementList.observeSource ++ matchToken(KW_END)) -->[RuleFunc] {
              case ((_, elseBody), _) => (condition, body) => IfElseExpr(condition, body, elseBody.map(_.toVector))
            } |
            (matchToken(KW_ELSIF) ++ ifRulePart.observeSource) -->[RuleFunc] {
              case (_, elseExpr) => (condition, body) => IfElseExpr(condition, body, WithSource(Vector(elseExpr), elseExpr.location))
            }
        )
      ) --> { case (((condition, _), body), ruleFunc) => ruleFunc(condition, body.map(_.toVector)) }

    matchToken(KW_IF) ++ ifRulePart --> { case (_, expr) => expr }
  }

  private val ruleExpressionMatch: TGrammar[Expr] = {
    val matchCaseRule: TGrammar[MatchExprCase] =
      rulePattern.observeSource ++ matchToken(OP_LAMBDA) ++ ruleStatementList.observeSource --> {
        case ((pattern, _), body) => MatchExprCase(pattern, body.map(_.toVector))
      }


    matchToken(KW_MATCH) ++ ruleExpression.observeSource ++ (matchCaseRule.observeSource*) ++ matchToken(KW_END) --> {
      case (((_, cmpValue), cases), _) =>
        MatchExpr(cmpValue, cases.toVector)
    }
  }

  private val ruleExpressionOther: TGrammar[Expr] =
    matchTokenFactory(Identifier) --> { case Identifier(id) => IdentifierExpr(id) : Expr } |
      matchTokenFactory(StringToken) --> {
        case StringToken(NonEmptyList(StringToken.StringPart(str), INil())) => StringValueExpr(str)
        case StringToken(NonEmptyList(StringToken.StringPart(str), ICons(_, _))) => ???
      } |
      matchTokenFactory(IntToken) --> { case IntToken(i) => IntValueExpr(i) } |
      matchToken(OP_OPENPAREN) ++ matchToken(OP_CLOSEPAREN) --> { _ => TupleExpr(Vector()) } |
      matchToken(OP_OPENPAREN) ++ ruleExpression ++ matchToken(OP_CLOSEPAREN) --> {
        case ((_, expr), _) => expr
      } |
      matchToken(KW_TRUE) --> { _ => BoolValueExpr(true) } |
      matchToken(KW_FALSE) --> { _ => BoolValueExpr(false) } |
      ruleIfExpr |
      ruleExpressionMatch

  private val ruleExpressionL20 = ruleExpressionOther


  private trait ParenCallHandlerBase {
    def apply[T](nextRule: TGrammar[T])(f: (WithSource[T], WithSource[Expr]) => T): TGrammar[T]
  }

  private object ParenCallHandler extends ParenCallHandlerBase {

    private val ruleArgList: TGrammar[Expr] =
      matchToken(OP_OPENPAREN) ++ (ruleExpression?) ++ matchToken(OP_CLOSEPAREN) --> {
        case ((_, Some(argList)), _) => argList
        case ((_, None), _) => TupleExpr(Vector.empty)
      }

    override def apply[T](nextRule: TGrammar[T])(f: (WithSource[T], WithSource[Expr]) => T): TGrammar[T] = {
      lazy val rule: TGrammar[T] =
        nextRule | rule.observeSource ++ ruleArgList.observeSource --> f.tupled

      rule
    }
  }

  private object SkipParenCallHandler extends ParenCallHandlerBase {
    override def apply[T](nextRule: TGrammar[T])(f: (WithSource[T], WithSource[Expr]) => T): TGrammar[T] = nextRule
  }


  private val ruleExpressionL19: TGrammar[Expr] =
    ParenCallHandler(ruleExpressionL20)(FunctionCallExpr.apply)

  private val ruleMemberAccess: TGrammar[WithSource[Expr] => Expr] =
    matchTokenFactory(Identifier) --> [WithSource[Expr] => Expr] { case Identifier(id) => baseExpr => DotExpr(baseExpr, id) } |
      matchToken(KW_NEW) --> { _ => ClassConstructorExpr.apply } |
      matchToken(KW_TYPE) --> { _ => TypeOfExpr.apply }

  private def ruleExpressionDot(nextRule: TGrammar[Expr], parenCallHandler: ParenCallHandlerBase): TGrammar[Expr] = {

    lazy val rule: TGrammar[Expr] =
      nextRule | rule.observeSource ++ matchToken(OP_DOT) ++ ruleMemberAccess --> {
        case ((baseExpr, _), memberAccessFunc) => memberAccessFunc(baseExpr)
      }

    rule
  }

  private val ruleExpressionL18: TGrammar[Expr] =
    ruleExpressionDot(ruleExpressionL19, ParenCallHandler)

  private val ruleExpressionL18_SkipParenCalls: TGrammar[Expr] =
    ruleExpressionDot(ruleExpressionL20, SkipParenCallHandler)

  private val ruleExpressionL17: TGrammar[Expr] = {
    def matchPrefxOp[TToken <: TokenWithCategory[_ <: TokenCategory] with UnaryOperatorToken : ClassTag](token: TToken): TGrammar[Expr] =
      matchToken(token) ++ ruleExpressionL17.observeSource --> { case (_, inner) => UnaryOperatorExpr(token.unaryOperator, inner) }

    ruleExpressionL18 |
      matchPrefxOp(OP_BITNOT) |
      matchPrefxOp(OP_BOOLNOT) |
      matchPrefxOp(OP_ADD) |
      matchPrefxOp(OP_SUB)
  }

  private val ruleExpressionL16: TGrammar[Expr] =
    ruleExpressionL17 | ruleExpressionL16.observeSource ++ ruleExpressionL18_SkipParenCalls.observeSource --> FunctionCallExpr.tupled

  private def createLeftAssociativeOperatorRule(nextGrammar: => TGrammar[Expr], grammar: => TGrammar[Expr])(opGrammars: TGrammar[BinaryOperator]*): TGrammar[Expr] =
    opGrammars.foldLeft(nextGrammar) { case (accum, opGrammar) =>
      accum | nextGrammar.observeSource ++ opGrammar ++ grammar.observeSource --> { case ((left, op), right) => BinaryOperatorExpr(op, left, right) }
    }

  private def ruleBinaryOperator[TToken <: TokenWithCategory[_ <: TokenCategory] with BinaryOperatorToken : ClassTag](token: TToken): TGrammar[BinaryOperator] =
    matchToken(token) --> { _ => token.binaryOperator }

  private val ruleExpressionL15: TGrammar[Expr] =
    createLeftAssociativeOperatorRule(ruleExpressionL16, ruleExpressionL15)(
      ruleBinaryOperator(OP_MUL),
      ruleBinaryOperator(OP_DIV),
    )

  private val ruleExpressionL14: TGrammar[Expr] =
    createLeftAssociativeOperatorRule(ruleExpressionL15, ruleExpressionL14)(
      ruleBinaryOperator(OP_ADD),
      ruleBinaryOperator(OP_SUB),
    )

  private val ruleExpressionL13: TGrammar[Expr] =
    createLeftAssociativeOperatorRule(ruleExpressionL14, ruleExpressionL13)(
      ruleBinaryOperator(OP_SHIFTLEFT),
      ruleBinaryOperator(OP_SHIFTRIGHT),
    )

  private val ruleExpressionL12: TGrammar[Expr] =
    createLeftAssociativeOperatorRule(ruleExpressionL13, ruleExpressionL12)(
      ruleBinaryOperator(OP_BITAND),
    )

  private val ruleExpressionL11: TGrammar[Expr] =
    createLeftAssociativeOperatorRule(ruleExpressionL12, ruleExpressionL11)(
      ruleBinaryOperator(OP_BITXOR),
    )

  private val ruleExpressionL10: TGrammar[Expr] =
    createLeftAssociativeOperatorRule(ruleExpressionL11, ruleExpressionL10)(
      ruleBinaryOperator(OP_BITOR),
    )

  private val ruleExpressionL09: TGrammar[Expr] =
    ruleExpressionL10 |
      matchToken(KW_TYPE) ++
        ((matchToken(OP_SUBTYPE) ++ ruleExpressionL10.observeSource --> { case (_, expr) => expr })?) ++
        ((matchToken(OP_SUPERTYPE) ++ ruleExpressionL10.observeSource --> { case (_, expr) => expr })?) ++
        ((matchToken(OP_COLON) ++ ruleExpressionL10.observeSource --> { case (_, expr) => expr })?) --> {
        case (((_, subtypeOf), supertypeOf), instanceType) =>
          TypeExpr(instanceType, subtypeOf, supertypeOf)
      }

  private lazy val rulePattern: TGrammar[Pattern] = ???


  private lazy val ruleExpression: TGrammar[Expr] = ???
  private lazy val ruleExpressionStatement: TGrammar[Expr] = ???
  private lazy val ruleStatementList: TGrammar[IList[WithSource[Stmt]]] = ???

}
