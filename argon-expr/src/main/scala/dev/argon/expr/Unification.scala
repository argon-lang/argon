package dev.argon.expr

import dev.argon.ast.IdentifierExpr
import dev.argon.util.{*, given}
import zio.*

import scala.compiletime.{erasedValue, error, summonInline}

private[expr] sealed trait Unification[R, E] {
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


    private given Comparer[Expr] = unify
    private given Comparer[Builtin] = autoComparer
    private given Comparer[LocalVar] = autoComparer
    private given Comparer[Var] = autoComparer

    // Needed to make autoComparer for Expr happy, even though it will not be used.
    private given Comparer[Hole] = EqualComparer[Hole]
    private given Comparer[Function] = EqualComparer[Function]

    private given Comparer[UniqueIdentifier] = EqualComparer[UniqueIdentifier]
    private given Comparer[NullaryBuiltin] = EqualComparer[NullaryBuiltin]
    private given Comparer[UnaryBuiltin] = EqualComparer[UnaryBuiltin]
    private given Comparer[BinaryBuiltin] = EqualComparer[BinaryBuiltin]
    private given Comparer[IdentifierExpr] = EqualComparer[IdentifierExpr]

    def unify(a: Expr, b: Expr): ZIO[R, E, Boolean] =
      (a, b) match {
        case (Expr.Error(), _) | (_, Expr.Error()) => ZIO.succeed(false)

        case (Expr.Hole(a), _) => unifyHole(a, b)
        case (_, Expr.Hole(b)) => unifyHole(b, a)
        
        case _ =>
          autoComparer[Expr].compare(a, b)
      }
  }

  export Matcher.unify





  private def unifyHole(hole: Hole, expr: Expr): ZIO[R, E, Boolean] =
    expr match {
      case Expr.Hole(other) if hole == other => ZIO.succeed(true)
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


  private def containsHole(e: Expr, h: Hole): Boolean =
    new HoleScanner {
      override val exprContext: Unification.this.exprContext.type = Unification.this.exprContext
      override val searchTarget: Hole = h
    }.exprScanner.scan(e).isLeft

}

object Unification {
  def unify[R, E](ec: ExprContext)(m: Ref[ec.Model])(a: ec.Expr, b: ec.Expr): ZIO[R, E, Boolean] =
    new Unification[R, E] {
      override val exprContext: ec.type = ec
      override protected val model: Ref[exprContext.Model] = m
    }.unify(a, b)
}
