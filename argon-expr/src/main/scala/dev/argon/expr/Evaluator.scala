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
           Expr.AnyType() |
           Expr.Box(_, _) |
           Expr.BoolLiteral(_) | Expr.IntLiteral(_) | Expr.StringLiteral(_) |
           Expr.Builtin(
             Builtin.Nullary(_) | Builtin.EqualTo(_, _, _) | Builtin.EqualToRefl(_, _)
           ) |
           Expr.Boxed(_) |
           Expr.InstanceSingletonType(_, _) |
           Expr.Lambda(_, _, _) |
           Expr.NewInstance(_, _) |
           Expr.RecordType(_, _) | Expr.RecordLiteral(_, _) |
           Expr.RefCellType(_) |
           Expr.EnumType(_, _) | Expr.EnumVariantLiteral(_, _, _, _) |
           Expr.TraitType(_, _) |
           Expr.Tuple(_) |
           Expr.TypeN(_) | Expr.TypeBigN(_) | Expr.FunctionType(_, _) |
           Expr.Variable(_) => ZIO.succeed(expr)

      case Expr.Sequence(Seq(), e) => normalizeToValue(e, fuel)

      case Expr.BindVariable(_, _) |
           Expr.Sequence(_, _) |
           Expr.VariableStore(_, _) |
           Expr.RefCellCreate(_, _) |
           Expr.RefCellLoad(_) |
           Expr.RefCellStore(_, _) |
           _: Expr.RecordFieldStore |
           _: Expr.Is => ???

      case Expr.And(Expr.BoolLiteral(false), _) => ZIO.succeed(Expr.BoolLiteral(false))
      case Expr.And(_, Expr.BoolLiteral(false)) => ZIO.succeed(Expr.BoolLiteral(false))
      case Expr.And(Expr.BoolLiteral(true), Expr.BoolLiteral(true)) => ZIO.succeed(Expr.BoolLiteral(true))
      case Expr.And(_, _) => ZIO.succeed(expr)
      
      case Expr.Or(Expr.BoolLiteral(true), _) => ZIO.succeed(Expr.BoolLiteral(true))
      case Expr.Or(_, Expr.BoolLiteral(true)) => ZIO.succeed(Expr.BoolLiteral(true))
      case Expr.Or(Expr.BoolLiteral(false), Expr.BoolLiteral(false)) => ZIO.succeed(Expr.BoolLiteral(false))
      case Expr.Or(_, _) => ZIO.succeed(expr)
      
      
      case ifElse: Expr.IfElse =>
        normalizeToValue(ifElse.condition, fuel).flatMap {
          case Expr.BoolLiteral(true) =>
            normalizeToValue(ifElse.trueBody, fuel)

          case Expr.BoolLiteral(false) =>
            normalizeToValue(ifElse.falseBody, fuel)

          case cond => ZIO.succeed(ifElse.copy(condition = cond))
        }

      case _: Expr.Match => ??? 

      case Expr.Hole(hole) =>
        normalizeHole(hole).flatMap { expr2 =>
          if expr2 == expr || fuel.isEmpty then
            ZIO.succeed(expr2)
          else
            normalizeToValue(expr2, fuel.consume)
        }

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

      case Expr.Finally(action, _) =>
        normalizeToValue(action, fuel)

      case Expr.FunctionCall(f, args) =>
        ZIO.foreach(args) { arg =>
          normalizeToValue(arg, fuel)
        }.flatMap { args =>
          getFunctionBody(f, args, fuel).flatMap { (callExpr, couldInline) =>
            if couldInline then
              normalizeToValue(callExpr, fuel.consume)
            else
              ZIO.succeed(Expr.FunctionCall(f, args))

          }
        }

      case Expr.FunctionObjectCall(f, a) => ???
      case Expr.InstanceMethodCall(_, _, _, _) => ???

      case Expr.TupleElement(index, tuple) =>
        normalizeToValue(tuple, fuel).flatMap {
          case Expr.Tuple(items) => normalizeToValue(items(index), fuel)
          case tuple => ZIO.succeed(Expr.TupleElement(index, tuple))
        }

      case Expr.RecordFieldLoad(rec, field, recordExpr) =>
        normalizeToValue(recordExpr, fuel).flatMap {
          case Expr.RecordLiteral(_, fields) =>
            ZIO.succeed(fields.find(_.field == field).getOrElse { ??? }.value)

          case recValue => ZIO.succeed(Expr.RecordFieldLoad(rec, field, recValue))
        }

      case Expr.Unbox(_, value) =>
        normalizeToValue(value, fuel).flatMap {
          case Expr.Box(_, inner) => normalizeToValue(inner, fuel)
          case _ => ZIO.succeed(expr)
        }

    }

}
