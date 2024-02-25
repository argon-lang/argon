package dev.argon.expr

import dev.argon.util.{*, given}
import cats.*

trait HoleFiller[EC <: ExprContext] extends ContextShifter[Id] {
  override val ec1: EC
  override val ec2: ec1.type
  
  import ec1.*
  
  val model: Model

  override protected def shiftVar(v: Var): Var =
    v

  override protected def shiftLocalVar(v: LocalVar): LocalVar =
    v

  override protected def shiftHole(hole: Hole): WExpr =
    model.resolveHole(hole) match {
      case Some(e) => shiftExpr(e)
      case None => WExpr.Hole(hole)
    }
}
