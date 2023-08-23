package dev.argon.util

import scala.deriving.Mirror
import scala.quoted.*

object MacroUtils {

  def patternMatch[T: Type, SubTypes <: Tuple: Type, A: Type](expr: Expr[T])(f: [U] => (Expr[U], Type[U]) => Expr[A])(using q: Quotes): Expr[A] =
    import q.reflect.{*, given}

    def createPatternBranch[U: Type]: CaseDef =
      val sym = Symbol.newBind(Symbol.spliceOwner, "v", Flags.EmptyFlags, TypeRepr.of[U])
      val varExpr = Ref(sym).asExprOf[U]
      CaseDef(
        Bind(sym, Typed(Wildcard(), TypeTree.of[U])),
        None,
        Block(Nil, f[U](varExpr, Type.of[U]).asTerm)
      )
    end createPatternBranch

    val cases = tupleForeach[SubTypes, CaseDef]([U] => (uType: Type[U]) => createPatternBranch[U](using uType))

    Match(expr.asTerm, cases).asExprOf[A]
  end patternMatch

  private def tupleForeach[T <: Tuple : Type, A](f: [U] => Type[U] => A)(using q: Quotes): List[A] =
    Type.of[T] match {
      case '[h *: t] => f(Type.of[h]) :: tupleForeach[t, A](f)
      case '[EmptyTuple] => Nil
      case _ => throw new Exception(s"Unexpected type for tuple: ${Type.show[T]}")
    }
  end tupleForeach
}
