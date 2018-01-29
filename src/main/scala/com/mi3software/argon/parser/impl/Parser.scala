package com.mi3software.argon.parser.impl

import com.mi3software.argon.parser.impl.Token._
import com.mi3software.argon.parser._
import com.mi3software.argon.util.WithSource

import scala.reflect.ClassTag
import scala.language.postfixOps
import scalaz.IList

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
    matchTokenFactory(Identifier).map(_.name)



  private val ruleIdentifier: TGrammar[Option[String]] =
    tokenUnderscore.map { _ => None : Option[String] } |
      tokenIdentifier.map(Some.apply)


  // Expressions
  private val ruleIfExpr: TGrammar[Expr] = {

    type RuleFunc = (WithSource[Expr], WithSource[Vector[WithSource[Stmt]]]) => Expr

    lazy val ifRulePart: TGrammar[Expr] =
      (
        ruleExpressionStatement.observeSource ++ matchToken(KW_THEN) ++ ruleStatementList.observeSource ++ (
          matchToken(KW_END).map[RuleFunc] { _ => (condition, body) => IfExpr(condition, body) } |
            (matchToken(KW_ELSE) ++ ruleStatementList.observeSource ++ matchToken(KW_END)).map[RuleFunc] {
              case ((_, elseBody), _) => (condition, body) => IfElseExpr(condition, body, elseBody.map(_.toVector))
            } |
            (matchToken(KW_ELSIF) ++ ifRulePart.observeSource).map[RuleFunc] {
              case (_, elseExpr) => (condition, body) => IfElseExpr(condition, body, WithSource(Vector(elseExpr), elseExpr.location))
            }
        )
      ).map { case (((condition, _), body), ruleFunc) => ruleFunc(condition, body.map(_.toVector)) }

    (matchToken(KW_IF) ++ ifRulePart).map { case (_, expr) => expr }
  }

  private val ruleExpressionMatch: TGrammar[Expr] = {
    val matchCaseRule: TGrammar[MatchExprCase] =
      (rulePattern.observeSource ++ matchToken(OP_LAMBDA) ++ ruleStatementList.observeSource).map {
        case ((pattern, _), body) => MatchExprCase(pattern, body.map(_.toVector))
      }


    (matchToken(KW_MATCH) ++ ruleExpression.observeSource ++ (matchCaseRule.observeSource*) ++ matchToken(KW_END)).map {
      case (((_, cmpValue), cases), _) =>
        MatchExpr(cmpValue, cases.toVector)
    }
  }

  private lazy val rulePattern: TGrammar[Pattern] = ???


  private lazy val ruleExpression: TGrammar[Expr] = ???
  private lazy val ruleExpressionStatement: TGrammar[Expr] = ???
  private lazy val ruleStatementList: TGrammar[IList[WithSource[Stmt]]] = ???

}
