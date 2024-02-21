package dev.argon.expr

import zio.{ZIO, IO}
import dev.argon.util.{*, given}

trait Evaluator[-R, +E] {
  val exprContext: ExprContext
  import exprContext.*

  def getFunctionBody(function: TFunction, args: Seq[WrapExpr], fuel: Int): ZIO[R, E, Option[WrapExpr]]
  def getMethodBody(method: TMethod, instance: WrapExpr, args: Seq[WrapExpr], fuel: Int): ZIO[R, E, Option[WrapExpr]]

  def normalizeTopLevelWrap(expr: WrapExpr, fuel: Int): ZIO[R, E, WrapExpr] =
    expr match {
      case _ if fuel <= 0 => ZIO.succeed(expr)
      case WrapExpr.OfExpr(expr) => normalizeTopLevel(expr, fuel)
      case WrapExpr.OfHole(hole) => ZIO.succeed(expr)
    }

  def normalizeTopLevel(expr: ArExpr[ExprConstructor], fuel: Int): ZIO[R, E, WrapExpr] =
    (expr.constructor: expr.constructor.type & ExprConstructor) match {
      case ExprConstructor.EnsureExecuted => ???
      case ExprConstructor.IfElse(_, _) => ???
      case ExprConstructor.PatternMatch(_) => ???
      case ExprConstructor.BindVariable(_) => ???
      case ExprConstructor.Sequence => ???
      case ExprConstructor.StoreVariable(_) => ???

      case ctor: (expr.constructor.type & ExprConstructor.FunctionCall) =>
        val args = expr.getArgs(ctor)
        getFunctionBody(ctor.function, args, fuel - 1).flatMap {
          case Some(body) => normalizeTopLevelWrap(body, fuel - 1)
          case None => ZIO.succeed(WrapExpr.OfExpr(expr))
        }

      case ctor: (expr.constructor.type & ExprConstructor.MethodCall) =>
        val (instance, _, args) = expr.getArgs(ctor)
        getMethodBody(ctor.method, instance, args, fuel - 1).flatMap {
          case Some(body) => normalizeTopLevelWrap(body, fuel - 1)
          case None => ZIO.succeed(WrapExpr.OfExpr(expr))
        }

      case ctor: (expr.constructor.type & ExprConstructor.FunctionObjectCall.type) =>
        val (func, arg) = expr.getArgs(ctor)
        normalizeTopLevelWrap(func, fuel - 1).flatMap {
          case WrapExpr.OfExpr(body) =>
            body.constructor match {
              case ctor: (body.constructor.type & ExprConstructor.LoadLambda) =>
                normalizeTopLevelWrap(
                  substituteVariables(Map(ctor.argVariable -> arg))(body.getArgs(ctor)),
                  fuel - 1,
                )

              case _ => ZIO.succeed(WrapExpr.OfExpr(expr))
            }

          case _ => ZIO.succeed(WrapExpr.OfExpr(expr))
        }

      case ctor: (expr.constructor.type & ExprConstructor.LoadTupleElement) =>
        val tupleValue = expr.getArgs(ctor)
        normalizeTopLevelWrap(tupleValue, fuel - 1).map {
          case WrapExpr.OfExpr(tupleExpr) =>
            tupleExpr.constructor match {
              case tupleCtor: (tupleExpr.constructor.type & ExprConstructor.LoadTuple.type) =>
                val tupleArgs: Seq[WrapExpr] = tupleExpr.getArgs(tupleCtor)
                tupleArgs
                  .slice(ctor.index.toInt, ctor.index.toInt + 1)
                  .headOption
                  .getOrElse { WrapExpr.OfExpr(expr) }

              case _ => WrapExpr.OfExpr(expr)
            }

          case WrapExpr.OfHole(_) => WrapExpr.OfExpr(expr)
        }

      case ctor: (expr.constructor.type & ExprConstructor.Proving) =>
        normalizeTopLevelWrap(expr.getArgs(ctor), fuel - 1)

      case ctor: (expr.constructor.type & ExprConstructor.Builtin[n]) =>
        normalizeBuiltin(ctor.builtin, expr.getArgs(ctor), fuel - 1)

      case ExprConstructor.ClassConstructorCall(_) |
          ExprConstructor.LoadConstantBool(_) | ExprConstructor.LoadConstantInt(_) | ExprConstructor.LoadConstantString(_) |
          ExprConstructor.LoadLambda(
            _
          ) | ExprConstructor.LoadTuple | ExprConstructor.LoadVariable(_) |
          ExprConstructor.RaiseException |
          ExprConstructor.TypeN | ExprConstructor.OmegaTypeN(_) | ExprConstructor.AnyType |
          ExprConstructor.ClassType(_) | ExprConstructor.TraitType(_) |
          ExprConstructor.FunctionType | ExprConstructor.UnionType | ExprConstructor.IntersectionType |
          ExprConstructor.ExistentialType(_) |
          ExprConstructor.ConjunctionType | ExprConstructor.DisjunctionType |
          ExprConstructor.SubtypeWitnessType | ExprConstructor.EqualTo | ExprConstructor.AssumeErasedValue =>
        ZIO.succeed(WrapExpr.OfExpr(expr))
    }

  private def normalizeBuiltin[N <: Int](builtin: ArgonBuiltin[N], args: NList[N, WrapExpr], fuel: Int): ZIO[R, E, WrapExpr] =
    builtin match {
      case builtin: ArgonBuiltin.SimpleValue =>
        ZIO.succeed(WrapExpr.OfExpr(ArExpr[ExprConstructor.Builtin[0]](ExprConstructor.Builtin(builtin), NNil)))

      case builtin: ArgonBuiltin.UnaryOperation =>
        val a = args.head
        normalizeBuiltinUnaryOperation(builtin, a, fuel)

      case builtin: ArgonBuiltin.BinaryOperation =>
        val a = args.head
        val b = args.tail.head
        normalizeBuiltinBinaryOperation(builtin, a, b, fuel)
    }

  private def asIntValue(e: WrapExpr): Option[BigInt] =
    e match {
      case WrapExpr.OfExpr(arExpr) =>
        arExpr.constructor match {
          case ExprConstructor.LoadConstantInt(i) => Some(i)
          case _ => None
        }

      case WrapExpr.OfHole(_) => None
    }

  private def asStrValue(e: WrapExpr): Option[String] =
    e match {
      case WrapExpr.OfExpr(arExpr) =>
        arExpr.constructor match {
          case ExprConstructor.LoadConstantString(s) => Some(s)
          case _ => None
        }

      case WrapExpr.OfHole(_) => None
    }


  private def normalizeBuiltinUnaryOperation(builtin: ArgonBuiltin.UnaryOperation, a: WrapExpr, fuel: Int): ZIO[R, E, WrapExpr] =
    for
      aNorm <- normalizeTopLevelWrap(a, fuel)
    yield {
      def intOp(f: BigInt => BigInt): WrapExpr =
        asIntValue(aNorm) match {
          case Some(value) => WrapExpr.OfExpr(ArExpr(ExprConstructor.LoadConstantInt(f(value)), EmptyTuple))
          case None => WrapExpr.OfExpr(ArExpr[ExprConstructor.Builtin[1]](ExprConstructor.Builtin(builtin), NCons(aNorm, NNil)))
        }

      builtin match {
        case ArgonBuiltin.IntNegate => intOp(-_)
      }
    }

  private def normalizeBuiltinBinaryOperation(builtin: ArgonBuiltin.BinaryOperation, a: WrapExpr, b: WrapExpr, fuel: Int): ZIO[R, E, WrapExpr] =
    for
      aNorm <- normalizeTopLevelWrap(a, fuel)
      bNorm <- normalizeTopLevelWrap(b, fuel)
    yield {
      def intOp(f: (BigInt, BigInt) => BigInt): WrapExpr =
        (
          for
            aValue <- asIntValue(aNorm)
            bValue <- asIntValue(bNorm)
          yield WrapExpr.OfExpr(ArExpr(ExprConstructor.LoadConstantInt(f(aValue, bValue)), EmptyTuple))

        ).getOrElse(WrapExpr.OfExpr(ArExpr[ExprConstructor.Builtin[2]](ExprConstructor.Builtin(builtin), NCons(aNorm, NCons(bNorm, NNil)))))

      def intRelOp(f: (BigInt, BigInt) => Boolean): WrapExpr =
        (
          for
            aValue <- asIntValue(aNorm)
            bValue <- asIntValue(bNorm)
          yield WrapExpr.OfExpr(ArExpr(ExprConstructor.LoadConstantBool(f(aValue, bValue)), EmptyTuple))

          ).getOrElse(WrapExpr.OfExpr(ArExpr[ExprConstructor.Builtin[2]](ExprConstructor.Builtin(builtin), NCons(aNorm, NCons(bNorm, NNil)))))

      def strOp(f: (String, String) => String): WrapExpr =
        (
          for
            aValue <- asStrValue(aNorm)
            bValue <- asStrValue(bNorm)
          yield WrapExpr.OfExpr(ArExpr(ExprConstructor.LoadConstantString(f(aValue, bValue)), EmptyTuple))

          ).getOrElse(WrapExpr.OfExpr(ArExpr[ExprConstructor.Builtin[2]](ExprConstructor.Builtin(builtin), NCons(aNorm, NCons(bNorm, NNil)))))

      builtin match {
        case ArgonBuiltin.IntAdd => intOp(_ + _)
        case ArgonBuiltin.IntSub => intOp(_ - _)
        case ArgonBuiltin.IntMul => intOp(_ * _)
        case ArgonBuiltin.IntEQ => intRelOp(_ == _)
        case ArgonBuiltin.IntNE => intRelOp(_ != _)
        case ArgonBuiltin.IntLT => intRelOp(_ < _)
        case ArgonBuiltin.IntLE => intRelOp(_ <= _)
        case ArgonBuiltin.IntGT => intRelOp(_ > _)
        case ArgonBuiltin.IntGE => intRelOp(_ >= _)
        case ArgonBuiltin.StringConcat => strOp(_ + _)
      }
    }

  def substituteVariables(varMap: Map[TVariable, WrapExpr])(expr: WrapExpr): WrapExpr
}
