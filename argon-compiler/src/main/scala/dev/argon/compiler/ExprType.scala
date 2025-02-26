package dev.argon.compiler

import dev.argon.expr.*
import cats.Id
import zio.*

trait ExprType {
  val context: Context
  val exprContext: context.ArgonExprContext {

  }
  val sigContext: context.ArgonSignatureContextBase {
    val exprContext: ExprType.this.exprContext.type
  }

  import context.Comp
  import exprContext.Expr

  private def boolType = Expr.Builtin(exprContext.Builtin.Nullary(NullaryBuiltin.BoolType))
  private def intType = Expr.Builtin(exprContext.Builtin.Nullary(NullaryBuiltin.IntType))
  private def stringType = Expr.Builtin(exprContext.Builtin.Nullary(NullaryBuiltin.StringType))

  protected def getHoleType(hole: exprContext.Hole): Expr

  def getExprType(e: Expr): Comp[Expr] =
    e match {
      case Expr.Hole(hole) => ZIO.succeed(getHoleType(hole))

      case Expr.BindVariable(_, _) =>
        ZIO.succeed(Expr.Tuple(Seq()))

      case Expr.BoolLiteral(_) =>
        ZIO.succeed(boolType)

      case Expr.Builtin(exprContext.Builtin.Binary(builtin, _, _)) =>
        ZIO.succeed(builtin match {
          case BinaryBuiltin.IntAdd | BinaryBuiltin.IntSub | BinaryBuiltin.IntMul |
               BinaryBuiltin.IntBitAnd | BinaryBuiltin.IntBitOr | BinaryBuiltin.IntBitXOr |
               BinaryBuiltin.IntBitShiftLeft | BinaryBuiltin.IntBitShiftRight =>
            intType

          case BinaryBuiltin.IntEQ | BinaryBuiltin.IntNE |
               BinaryBuiltin.IntLT | BinaryBuiltin.IntLE |
               BinaryBuiltin.IntGT | BinaryBuiltin.IntGE |
               BinaryBuiltin.StringEQ | BinaryBuiltin.StringNE =>
            boolType

          case BinaryBuiltin.StringConcat => stringType
          case _ =>
            println("Unimplemented getExprType binary builtin: " + builtin)
            ???
        })

      case Expr.Builtin(exprContext.Builtin.Unary(builtin, _)) =>
        ZIO.succeed(builtin match {
          case UnaryBuiltin.IntNegate | UnaryBuiltin.IntBitNot => intType
        })

      case Expr.Builtin(exprContext.Builtin.Nullary(builtin)) =>
        ZIO.succeed(builtin match {
          case NullaryBuiltin.BoolType | NullaryBuiltin.IntType |
               NullaryBuiltin.StringType | NullaryBuiltin.NeverType =>
            Expr.TypeN(Expr.IntLiteral(0))
        })

      case Expr.FunctionCall(f, args) =>
        for
          sig <- f.signature
        yield sigContext.signatureFromDefault(sig).returnTypeForArgs(
          exprContext.ParameterOwner.Func(f),
          args
        )

      case Expr.IntLiteral(_) =>
        ZIO.succeed(intType)

      case Expr.Variable(v) =>
        ZIO.succeed(v.varType)

      case Expr.StringLiteral(_) =>
        ZIO.succeed(stringType)

      case Expr.Tuple(items) =>
        for
          itemTypes <- ZIO.foreach(items)(getExprType)
        yield Expr.Tuple(itemTypes)

      case Expr.TupleElement(index, tuple) =>
        getExprType(tuple).flatMap {
          case Expr.Tuple(itemTypes) => ZIO.succeed(itemTypes(index))
          case _ => ???
        }

      case _ =>
        ZIO.logError("Unimplemented getExprType expression: " + e).as(???)
    }


}
