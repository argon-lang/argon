package dev.argon.expr

import dev.argon.util.Fuel
import zio.*

trait Evaluator[R, E] {
  val exprContext: ExprContext
  import exprContext.*

  def getFunctionBody(function: Function, args: Seq[Expr], fuel: Fuel): ZIO[R, E, (Expr, Boolean)]
  def normalizeHole(hole: Hole): ZIO[R, E, Expr]


  def normalizeToValue(expr: Expr, fuel: Fuel): ZIO[R, E, Expr] =
    expr match {
      // These expressions always construct values or otherwise have no remaining normalizing to perform
      case Expr.Error() |
        Expr.ErasedValue() |
        Expr.BoolLiteral(_) | Expr.IntLiteral(_) | Expr.StringLiteral(_) |
        Expr.Builtin(
          Builtin.Nullary(_) | Builtin.EqualTo(_, _, _) | Builtin.EqualToRefl(_, _)
        ) |
        Expr.Lambda(_, _) |
        Expr.RecordType(_, _) | Expr.RecordLiteral(_, _) |
        Expr.Tuple(_) |
        Expr.TypeN(_) | Expr.TypeBigN(_) | Expr.FunctionType(_, _) |
        Expr.Variable(_) => ZIO.succeed(expr)

      case Expr.BindVariable(_, _) | Expr.Sequence(_, _) | Expr.StoreVariable(_, _) | (_: Expr.IfElse) => ???

      case Expr.Hole(hole) => normalizeHole(hole)

      case Expr.Builtin(Builtin.Unary(op, a)) =>
        for
          a <- normalizeToValue(a, fuel)
        yield (op, a) match {
          case (UnaryBuiltin.IntNegate, Expr.IntLiteral(i)) => Expr.IntLiteral(-i)
          case _ => Expr.Builtin(Builtin.Unary(op, a))
        }

      case Expr.Builtin(Builtin.Binary(op, a, b)) =>
        for
          a <- normalizeToValue(a, fuel)
          b <- normalizeToValue(b, fuel)
        yield (op, a, b) match {
          case (BinaryBuiltin.IntAdd, Expr.IntLiteral(a), Expr.IntLiteral(b)) => Expr.IntLiteral(a + b)
          case (BinaryBuiltin.IntSub, Expr.IntLiteral(a), Expr.IntLiteral(b)) => Expr.IntLiteral(a - b)
          case (BinaryBuiltin.IntMul, Expr.IntLiteral(a), Expr.IntLiteral(b)) => Expr.IntLiteral(a * b)
          case (BinaryBuiltin.IntEQ, Expr.IntLiteral(a), Expr.IntLiteral(b)) => Expr.BoolLiteral(a == b)
          case (BinaryBuiltin.IntNE, Expr.IntLiteral(a), Expr.IntLiteral(b)) => Expr.BoolLiteral(a != b)
          case (BinaryBuiltin.IntLT, Expr.IntLiteral(a), Expr.IntLiteral(b)) => Expr.BoolLiteral(a < b)
          case (BinaryBuiltin.IntLE, Expr.IntLiteral(a), Expr.IntLiteral(b)) => Expr.BoolLiteral(a <= b)
          case (BinaryBuiltin.IntGT, Expr.IntLiteral(a), Expr.IntLiteral(b)) => Expr.BoolLiteral(a > b)
          case (BinaryBuiltin.IntGE, Expr.IntLiteral(a), Expr.IntLiteral(b)) => Expr.BoolLiteral(a >= b)
          case (BinaryBuiltin.StringConcat, Expr.StringLiteral(a), Expr.StringLiteral(b)) => Expr.StringLiteral(a + b)
          case _ => Expr.Builtin(Builtin.Binary(op, a, b))
        }

      case Expr.FunctionCall(f, args) =>
        getFunctionBody(f, args, fuel).flatMap { (callExpr, couldInline) =>
          if couldInline then
            normalizeToValue(callExpr, fuel.consume)
          else
            ZIO.succeed(callExpr)
        }

      case Expr.FunctionObjectCall(f, a) => ???

      case Expr.TupleElement(index, tuple) =>
        normalizeToValue(tuple, fuel).flatMap {
          case Expr.Tuple(items) => normalizeToValue(items(index), fuel)
          case tuple => ZIO.succeed(Expr.TupleElement(index, tuple))
        }

      case Expr.RecordFieldLoad(rec, field, recordExpr) =>
        normalizeToValue(recordExpr, fuel).flatMap {
          case Expr.RecordLiteral(_, fields) =>
            ZIO.succeed(fields.find(_.name == getRecordFieldName(field)).getOrElse { ??? }.value)

          case recValue => ZIO.succeed(Expr.RecordFieldLoad(rec, field, recValue))
        }

    }

}
