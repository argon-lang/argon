package dev.argon.expr

import dev.argon.util.{TreeShifter, UniqueIdentifier}
import zio.{UIO, ZIO}
import zio.stm.TMap
import zio.interop.catz.core.given

private abstract class FreshVariableShifter[EC <: ExprContext](val exprContext: EC) extends ExprShifter[UIO] {
  import exprContext.*

  override final val ec1: exprContext.type = exprContext
  override final val ec2: exprContext.type = exprContext

  protected val varMapping: TMap[LocalVar, LocalVar]
  protected val lambdaVarMapping: TMap[LambdaParameterVar, LambdaParameterVar]

  override protected def varShifter: Shifter[Var, Var] = {
    case a: LocalVar =>
      varMapping.getOrElse(a, a).commit

    case a: LambdaParameterVar =>
      lambdaVarMapping.getOrElse(a, a).commit

    case a => ZIO.succeed(a)
  }

  override protected def localVarShifter: Shifter[LocalVar, LocalVar] = a =>
    for
      id <- UniqueIdentifier.make
      v = a.copy(id = id)
      _ <- varMapping.put(a, v).commit
    yield v

  override protected def lambdaVarShifter: Shifter[LambdaParameterVar, LambdaParameterVar] = a =>
    for
      id <- UniqueIdentifier.make
      v = a.copy(id = id)
      _ <- lambdaVarMapping.put(a, v).commit
    yield v

  override protected def shiftHole(hole: Hole): UIO[Expr] =
    ZIO.succeed(Expr.Hole(hole))

  final def substitute(e: Expr): UIO[Expr] =
    exprShifter.shift(e)

}

object FreshVariableShifter {
  def substitute(ec: ExprContext)(e: ec.Expr): UIO[ec.Expr] =
    for
      mapping <- TMap.empty[ec.LocalVar, ec.LocalVar].commit
      lambdaMapping <- TMap.empty[ec.LambdaParameterVar, ec.LambdaParameterVar].commit
      res <- new FreshVariableShifter[ec.type](ec) {
        import ec.*
        override protected val varMapping: TMap[LocalVar, LocalVar] = mapping
        override protected val lambdaVarMapping: TMap[LambdaParameterVar, LambdaParameterVar] = lambdaMapping
      }.substitute(e)
    yield res
}

