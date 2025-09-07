package dev.argon.expr

import dev.argon.util.{TreeShifter, UniqueIdentifier}
import dev.argon.ast.IdentifierExpr
import cats.*

private trait HoleSubstitution {
  val exprContext: ExprContext
  import exprContext.*

  protected val holeMapping: Map[Hole, Expr]

  private object ShiftHelper extends TreeShifter[Id] {
    import StandardShifters.given

    given exprShifter: Shifter[Expr, Expr]:
      override def shift(a: Expr): Expr =
        (a match {
          case Expr.Hole(h) => holeMapping.get(h)
          case _ => None
        }).getOrElse(autoShifter[Expr, Expr].shift(a))
    end exprShifter

    given Shifter[Expr.RecordType, Expr.RecordType] = autoShifter
    given Shifter[Expr.EnumType, Expr.EnumType] = autoShifter
    given Shifter[RecordFieldLiteral, RecordFieldLiteral] = autoShifter

    given Shifter[Builtin, Builtin] = autoShifter
    given Shifter[Hole, Hole] = identityShifter


    given Shifter[Var, Var] = autoShifter
    given Shifter[LocalVar, LocalVar] = autoShifter

    given Shifter[Function, Function] = identityShifter
    given Shifter[Record, Record] = identityShifter
    given Shifter[RecordField, RecordField] = identityShifter
    given Shifter[Enum, Enum] = identityShifter
    given Shifter[EnumVariant, EnumVariant] = identityShifter

    given Shifter[NullaryBuiltin, NullaryBuiltin] = identityShifter
    given Shifter[UnaryBuiltin, UnaryBuiltin] = identityShifter
    given Shifter[BinaryBuiltin, BinaryBuiltin] = identityShifter
    given Shifter[ParameterOwner, ParameterOwner] = identityShifter

    given Shifter[IdentifierExpr, IdentifierExpr] = identityShifter
    given Shifter[UniqueIdentifier, UniqueIdentifier] = identityShifter
  }

  final def substitute(e: Expr): Expr =
    ShiftHelper.exprShifter.shift(e)

}

object HoleSubstitution {
  def substitute(ec: ExprContext)(mapping: Map[ec.Hole, ec.Expr])(e: ec.Expr): ec.Expr =
    new HoleSubstitution {
      override val exprContext: ec.type = ec
      override protected val holeMapping: Map[exprContext.Hole, exprContext.Expr] = mapping
    }.substitute(e)
}

