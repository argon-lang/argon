package dev.argon.expr

import dev.argon.util.{TreeShifter, UniqueIdentifier}
import cats.*
import dev.argon.ast.IdentifierExpr

private trait Substitution {
  val exprContext: ExprContext
  import exprContext.*

  protected val variableMapping: Map[Var, Expr]

  private object ShiftHelper extends TreeShifter[Id] {
    import StandardShifters.given

    given exprShifter: Shifter[Expr, Expr]:
      override def shift(a: Expr): Expr =
        (a match {
          case Expr.Variable(v) =>
            variableMapping.get(v)
          case _ => None
        }).getOrElse(autoShifter[Expr, Expr].shift(a))
    end exprShifter

    given Shifter[Pattern, Pattern] = autoShifter
    given Shifter[Expr.RecordType, Expr.RecordType] = autoShifter
    given Shifter[Expr.EnumType, Expr.EnumType] = autoShifter
    given Shifter[RecordFieldLiteral, RecordFieldLiteral] = autoShifter
    given Shifter[RecordFieldPattern, RecordFieldPattern] = autoShifter

    given Shifter[Builtin, Builtin] = autoShifter
    given Shifter[Hole, Hole] = identity


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

object Substitution {
  def substitute(ec: ExprContext)(mapping: Map[ec.Var, ec.Expr])(e: ec.Expr): ec.Expr =
    new Substitution {
      override val exprContext: ec.type = ec
      override protected val variableMapping: Map[exprContext.Var, exprContext.Expr] = mapping
    }.substitute(e)
}

