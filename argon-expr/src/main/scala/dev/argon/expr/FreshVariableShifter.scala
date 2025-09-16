package dev.argon.expr

import dev.argon.util.{TreeShifter, UniqueIdentifier}
import dev.argon.ast.IdentifierExpr
import cats.*
import cats.data.State
import zio.{UIO, ZIO}
import zio.stm.TMap
import zio.interop.catz.core.given

private trait FreshVariableShifter {
  val exprContext: ExprContext
  import exprContext.*

  protected val varMapping: TMap[LocalVar, LocalVar]

  private object ShiftHelper extends TreeShifter[UIO] {
    import StandardShifters.given

    given exprShifter: Shifter[Expr, Expr] = autoShifter
    given Shifter[Pattern, Pattern] = autoShifter

    given Shifter[Expr.RecordType, Expr.RecordType] = autoShifter
    given Shifter[Expr.EnumType, Expr.EnumType] = autoShifter
    given Shifter[RecordFieldLiteral, RecordFieldLiteral] = autoShifter
    given Shifter[RecordFieldPattern, RecordFieldPattern] = autoShifter
    given Shifter[MatchCase, MatchCase] = autoShifter

    given Shifter[Builtin, Builtin] = autoShifter
    given Shifter[Hole, Hole] = identityShifter


    given Shifter[Var, Var]:
      override def shift(a: Var): UIO[Var] =
        a match {
          case a: LocalVar =>
            varMapping.getOrElse(a, a).commit

          case _ => ZIO.succeed(a)
        }
    end given

    given Shifter[LocalVar, LocalVar]:
      override def shift(a: LocalVar): UIO[LocalVar] = {
        for
          id <- UniqueIdentifier.make
          v = a.copy(id = id)
          _ <- varMapping.put(a, v).commit
        yield v
      }
    end given

    given Shifter[Function, Function] = identityShifter
    given Shifter[Record, Record] = identityShifter
    given Shifter[RecordField, RecordField] = identityShifter
    given Shifter[Enum, Enum] = identityShifter
    given Shifter[EnumVariant, EnumVariant] = identityShifter
    given Shifter[Trait, Trait] = identityShifter

    given Shifter[NullaryBuiltin, NullaryBuiltin] = identityShifter
    given Shifter[UnaryBuiltin, UnaryBuiltin] = identityShifter
    given Shifter[BinaryBuiltin, BinaryBuiltin] = identityShifter
    given Shifter[ExpressionOwner, ExpressionOwner] = identityShifter

    given Shifter[IdentifierExpr, IdentifierExpr] = identityShifter
    given Shifter[UniqueIdentifier, UniqueIdentifier] = identityShifter
  }

  final def substitute(e: Expr): UIO[Expr] =
    ShiftHelper.exprShifter.shift(e)

}

object FreshVariableShifter {
  def substitute(ec: ExprContext)(e: ec.Expr): UIO[ec.Expr] = {
    TMap.empty[ec.LocalVar, ec.LocalVar].commit.flatMap { mapping =>
      new FreshVariableShifter {
        override val exprContext: ec.type = ec
        override protected val varMapping: TMap[exprContext.LocalVar, exprContext.LocalVar] = mapping
      }.substitute(e)
    }
  }
}

