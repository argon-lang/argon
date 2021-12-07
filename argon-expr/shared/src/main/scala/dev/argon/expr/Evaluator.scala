package dev.argon.expr

import zio.{ZIO, IO}
import dev.argon.util.{_, given}

trait Evaluator[R, E] {
  val exprContext: ExprContext
  import exprContext._

  def getFunctionBody(function: TFunction, args: Vector[WrapExpr], fuel: Int): ZIO[R, E, Option[WrapExpr]]
  def getMethodBody(method: TMethod, instance: WrapExpr, args: Vector[WrapExpr], fuel: Int): ZIO[R, E, Option[WrapExpr]]

  def normalizeTopLevelWrap(expr: WrapExpr, fuel: Int): ZIO[R, E, WrapExpr] = expr match {
    case _ if fuel <= 0 => IO.succeed(expr)
    case WrapExpr.OfExpr(expr) => normalizeTopLevel(expr, fuel)
    case WrapExpr.OfHole(hole) => IO.succeed(expr)
  }

  def normalizeTopLevel(expr: ArExpr[ExprConstructor], fuel: Int): ZIO[R, E, WrapExpr] = (expr.constructor : expr.constructor.type & ExprConstructor) match {
    case ExprConstructor.EnsureExecuted => ???
    case ExprConstructor.IfElse => ???
    case ExprConstructor.PatternMatch(_) => ???
    case ExprConstructor.LetBinding(_) => ???
    case ExprConstructor.Sequence => ???
    case ExprConstructor.StoreVariable(_) => ???

    case ctor: (expr.constructor.type & ExprConstructor.FunctionCall) =>
      val args = expr.getArgs(ctor)
      getFunctionBody(ctor.function, args, fuel - 1).flatMap {
        case Some(body) => normalizeTopLevelWrap(body, fuel - 1)
        case None => IO.succeed(WrapExpr.OfExpr(expr))
      }

    case ctor: (expr.constructor.type & ExprConstructor.MethodCall) =>
      val (instance, _, args) = expr.getArgs(ctor)
      getMethodBody(ctor.method, instance, args, fuel - 1).flatMap {
        case Some(body) => normalizeTopLevelWrap(body, fuel - 1)
        case None => IO.succeed(WrapExpr.OfExpr(expr))
      }

    case ctor: (expr.constructor.type & ExprConstructor.FunctionObjectCall.type) =>
      val (func, arg) = expr.getArgs(ctor)
      normalizeTopLevelWrap(func, fuel - 1).flatMap {
        case WrapExpr.OfExpr(body) =>
          body.constructor match {
            case ExprConstructor.LoadLambda(argVariable) =>
              normalizeTopLevelWrap(substituteVariables(Map(argVariable -> arg))(expr.args.asInstanceOf[WrapExpr]), fuel - 1)

            case _ => IO.succeed(WrapExpr.OfExpr(expr))
          }

        case _ => IO.succeed(WrapExpr.OfExpr(expr))
      }

    case ctor: (expr.constructor.type & ExprConstructor.LoadTupleElement) =>
      val tupleValue = expr.getArgs(ctor)
      normalizeTopLevelWrap(tupleValue, fuel - 1).map {
        case WrapExpr.OfExpr(tupleExpr) =>
          tupleExpr.constructor match {
            case tupleCtor: (tupleExpr.constructor.type & ExprConstructor.LoadTuple.type) =>
              val tupleArgs: NonEmptyList[WrapExpr] = tupleExpr.getArgs(tupleCtor)
              tupleArgs.toList
                .slice(ctor.index, ctor.index + 1)
                .headOption
                .getOrElse { WrapExpr.OfExpr(expr) }

            case _ => WrapExpr.OfExpr(expr)
          }

        case WrapExpr.OfHole(_) => WrapExpr.OfExpr(expr)
      }

    case ExprConstructor.ClassConstructorCall(_) | ExprConstructor.DataConstructorCall(_) |
        ExprConstructor.LoadConstantBool(_) | ExprConstructor.LoadConstantInt(_) | ExprConstructor.LoadConstantString(_) |
        ExprConstructor.LoadLambda(_) | ExprConstructor.LoadTuple | ExprConstructor.LoadUnit | ExprConstructor.LoadVariable(_) |
        ExprConstructor.TypeN | ExprConstructor.OmegaTypeN(_) | ExprConstructor.AnyType |
        ExprConstructor.ClassType(_) | ExprConstructor.TraitType(_) | ExprConstructor.DataConstructorType(_) |
        ExprConstructor.FunctionType | ExprConstructor.UnionType | ExprConstructor.IntersectionType |
        ExprConstructor.ExistentialType(_) | ExprConstructor.NeverType |
        ExprConstructor.ConjunctionType | ExprConstructor.DisjunctionType |
        ExprConstructor.SubtypeWitnessType | ExprConstructor.EqualTo | ExprConstructor.AssumeErasedValue =>
      IO.succeed(WrapExpr.OfExpr(expr))
  }

  def substituteVariables(varMap: Map[TVariable, WrapExpr])(expr: WrapExpr): WrapExpr = ???
}
