package dev.argon.expr

import dev.argon.util.UniqueIdentifier
import zio.stream.*
import zio.*
import zio.test.{Gen, Sample}

class TestResolver[R] extends ImplicitResolver[R, String] {
  override val exprContext: TestExprContext.type = TestExprContext
  import exprContext.*

  override def createHole: ZIO[R, String, THole] = UniqueIdentifier.make

  val traitA = "TraitA"
  val traitB = "TraitB"
  val traitC = "TraitC"
  val traitD = "TraitD"
  val traitE = "TraitE"

  def traitType(t: String): WrapExpr = WrapExpr.OfExpr(ArExpr(ExprConstructor.TraitType(t), Vector()))

  def genTraitType: Gen[Any, WrapExpr] =
    Gen(ZStream(traitA, traitB, traitC, traitD, traitE).map { t => Some(Sample(traitType(t), ZStream.empty)) })

  private def impliesFalse(p: WrapExpr): WrapExpr =
    wrapExpr(ExprConstructor.FunctionType, (p, wrapExpr(ExprConstructor.NeverType, EmptyTuple)))

  protected override def isSubClass(classA: TClass, aArgs: Seq[WrapExpr], classB: TClass, bArgs: Seq[WrapExpr], fuel: Int)
    : ZIO[R, String, SubClassResult] =
    ZIO.succeed(SubClassResult.NotSubClassProof(wrapExpr(
      ExprConstructor.AssumeErasedValue,
      EmptyTuple,
    )))

  private val subTraits: Seq[(String, String)] =
    Seq(
      traitB -> traitA,
      traitC -> traitB,
      traitD -> traitA,
      traitE -> traitB,
      traitE -> traitD,
    )

  private def checkSubTraits(t1: String, t2: String): Boolean =
    subTraits.exists { case (st1, st2) =>
      t1 == st1 && (t2 == st2 || checkSubTraits(st2, t2))
    }

  protected override def isSubTrait(traitA: TTrait, aArgs: Seq[WrapExpr], traitB: TTrait, bArgs: Seq[WrapExpr], fuel: Int)
    : ZIO[R, String, SubClassResult] =
    if checkSubTraits(traitA, traitB) then {
      ZIO.succeed(SubClassResult.SubClassProof(wrapExpr(
        ExprConstructor.AssumeErasedValue,
        EmptyTuple,
      )))
    }
    else {
      ZIO.succeed(SubClassResult.NotSubClassProof(wrapExpr(
        ExprConstructor.AssumeErasedValue,
        EmptyTuple,
      )))
    }

  protected override def classImplementsTrait
    (classA: TClass, aArgs: Seq[WrapExpr], traitB: TTrait, bArgs: Seq[WrapExpr], fuel: Int)
    : ZIO[R, String, SubClassResult] =
    ZIO.succeed(SubClassResult.NotSubClassProof(wrapExpr(
      ExprConstructor.AssumeErasedValue,
      EmptyTuple,
    )))

  protected override def traitRelations(arTrait: String): ZIO[R, String, Seq[ExprRelation]] = ZIO.succeed(Seq.empty)

  protected override def classRelations(arClass: String): ZIO[R, String, Seq[ExprRelation]] = ZIO.succeed(Seq.empty)

  protected override def functionRelations(function: String): ZIO[R, String, Seq[ExprRelation]] =
    function match {
      case "natLessThan" => ZIO.succeed(Seq(ExprRelation.SyntacticEquality, ExprRelation.SyntacticEquality))
      case _ => ZIO.fail(s"Unknown function: $function")
    }

  protected override def methodRelations(method: String): ZIO[R, String, Seq[ExprRelation]] = ZIO.succeed(Seq.empty)

  protected override def classConstructorRelations(classCtor: String): ZIO[R, String, Seq[ExprRelation]] =
    ZIO.succeed(Seq.empty)

  protected override val natLessThanFunction: ZIO[R, String, String] = ZIO.succeed("natLessThan")
  protected override val boolType: ZIO[R, String, WrapExpr] =
    ZIO.succeed(wrapExpr(ExprConstructor.ClassType("Ar.Bool"), Vector()))

  protected override def invalidExpr: ZIO[R, String, Nothing] = ZIO.fail("Invalid expression")
  protected override def invalidPredicateExpr: ZIO[R, String, Nothing] = ZIO.fail("Invalid predicate expression")

  protected override val evaluator: Evaluator[R, String] { val exprContext: TestResolver.this.exprContext.type } =
    new TestEvaluator[R, String]

}
