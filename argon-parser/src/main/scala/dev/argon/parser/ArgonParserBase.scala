package dev.argon.parser

import dev.argon.ast.{Expr, FunctionParameterListType, StringFragment}
import dev.argon.parser.Token.{BinaryOperatorToken, UnaryOperatorToken}
import dev.argon.util.{Location, SourceLocation, WithSource, WithLocation as WithLoc}
import zio.{UIO, ZEnvironment, ZIO}

import scala.annotation.unused
import scala.reflect.TypeTest

private[parser] abstract class ArgonParserBase {
  protected type R
  protected type E >: SyntaxError

  protected final class ParseEffect[+A](val run: ZIO[R & LexerMode, E, WithLocation[A]]) {
    def map[B](f: A => B): ParseEffect[B] =
      ParseEffect(run.map(_.map(f)))

    def flatMap[B](f: A => ParseEffect[B]): ParseEffect[B] =
      ParseEffect(
        for
          aLoc <- run
          bLoc <- f(aLoc.value).run
        yield WithLoc(bLoc.value, Location.merge(aLoc.location, bLoc.location))
      )
  }
  
  protected type F[+A] = ParseEffect[A]
  protected type WithLocation[+A] = WithSource[A]
  protected type EOFToken = Token.EndOfFile.type

  protected def pure[A](a: A): F[A] = ParseEffect(currentLocation.map((WithLoc(a, _))))
  protected def withLocation[A](a: F[A]): F[WithSource[A]] =
    ParseEffect(a.run.map(aLoc => WithLoc(aLoc, aLoc.location)))

  protected def withLexMode[A](mode: LexerMode)(action: F[A]): F[A] =
    ParseEffect(action.run.provideSomeEnvironment[R](_ ++ ZEnvironment(mode)))

  protected def currentLocation: UIO[SourceLocation]
  protected def peek: F[Token]
  protected def terminal[T <: Token: TypeCategorySet](using TypeTest[Token, T])(using sourcecode.Name): F[T]
  protected def error[T <: Token: TypeCategorySet](using sourcecode.Name): F[Nothing]



  protected def const[A](a: A)(@unused ignored: Any*): A = a


  protected def unaryOp(op: UnaryOperatorToken[?], expr: WithSource[Expr]): Expr =
    Expr.UnaryOperation(op.unOperator, expr)

  protected def binaryOp(a: WithSource[Expr], op: BinaryOperatorToken[?], b: WithSource[Expr]): Expr =
    Expr.BinaryOperation(a, op.binOperator, b)

  protected def simplifyFragments(fragments: List[StringFragment]): List[StringFragment] =
    fragments match {
      case StringFragment.Text(s1) :: StringFragment.Text(s2) :: tail =>
        simplifyFragments(StringFragment.Text(s1 + s2) :: tail)

      case head :: tail =>
        head :: simplifyFragments(tail)

      case Nil => Nil
    }

  protected def curriedCall(func: WithSource[Expr], args: Seq[(FunctionParameterListType, WithSource[Expr])]): Expr =
    args.foldLeft(func) { case (func, (paramListType, arg)) =>
      WithLoc(
        Expr.FunctionCall(func, paramListType, arg),
        Location.merge(func.location, arg.location)
      )
    }.value

}


