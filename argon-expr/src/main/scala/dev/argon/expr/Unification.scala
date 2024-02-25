package dev.argon.expr

import dev.argon.util.{*, given}
import zio.*

import scala.compiletime.{error, erasedValue, summonInline}

trait Unification[R, E] {
  val exprContext: ExprContext
  import exprContext.{*, given}

  protected val model: Ref[Model]

  private object Matcher extends TreeComparison {
    import StandardComparers.given

    override type Comparison = ZIO[R, E, Boolean]

    override def comparisonFromBoolean(b: Boolean): Comparison =
      ZIO.succeed(b)

    override def combineComparison(a: => Comparison, b: => Comparison): Comparison =
      a && b

    override def combineAllComparisons[A](a: Seq[A])(f: A => ZIO[R, E, Boolean]): ZIO[R, E, Boolean] =
      ZIO.forall(a)(f)


    private given Comparer[WExpr] = unify
    private given Comparer[Expr] = autoComparer[Expr]
    private given Comparer[Builtin] = autoComparer[Builtin]
    private given Comparer[LocalVar] = unifyLocalVariable
    private given Comparer[Var] = unifyVariable

    private given Comparer[Function] = EqualComparer[Function]

    private given Comparer[NullaryBuiltin] = EqualComparer[NullaryBuiltin]
    private given Comparer[UnaryBuiltin] = EqualComparer[UnaryBuiltin]
    private given Comparer[BinaryBuiltin] = EqualComparer[BinaryBuiltin]

    def unify(a: WExpr, b: WExpr): ZIO[R, E, Boolean] =
      (a, b) match {
        case (WExpr.Error(), _) | (_, WExpr.Error()) => ZIO.succeed(false)
        case (WExpr.Normal(a), WExpr.Normal(b)) =>
          summon[Comparer[Expr]].compare(a, b)

        case (WExpr.Hole(a), _) => unifyHole(a, b)
        case (_, WExpr.Hole(b)) => unifyHole(b, a)
      }
  }

  export Matcher.unify

  protected def unifyLocalVariable(a: Var, b: Var): ZIO[R, E, Boolean]
  protected def unifyVariable(a: Var, b: Var): ZIO[R, E, Boolean]





  private def unifyHole(hole: Hole, expr: WExpr): ZIO[R, E, Boolean] =
    expr match {
      case WExpr.Hole(other) if hole == other => ZIO.succeed(true)
      case _ =>
        model.get.flatMap { m =>
          m.resolveHole(hole) match {
            case Some(holeExpr) => unify(holeExpr, expr)
            case None =>
              val filler = new HoleFiller[exprContext.type] {
                override val ec1: exprContext.type = exprContext
                override val ec2: exprContext.type = exprContext

                override val model: ec1.Model = m
              }

              val expr2 = filler.shiftExpr(expr)
              val isCircular = containsHole(expr2, hole)

              if isCircular then
                ZIO.succeed(false)
              else
                model.set(m.addMapping(hole, expr2)).as(true)
          }
        }
    }


  private def containsHole(e: WExpr, h: Hole): Boolean =
    new HoleScanner {
      override val exprContext: Unification.this.exprContext.type = Unification.this.exprContext
      override val searchTarget: Hole = h
    }.wexprScanner.scan(e).isLeft

}
