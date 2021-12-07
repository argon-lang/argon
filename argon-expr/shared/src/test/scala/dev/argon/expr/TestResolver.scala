package dev.argon.expr

import dev.argon.util.UniqueIdentifier
import zio.stream.Stream
import zio.{IO, ZIO}
import zio.test.{Gen, Sample}

class TestResolver[R <: zio.Random] extends ImplicitResolver[R, String] {
  override val exprContext: TestExprContext.type = TestExprContext
  import exprContext._

  override def createHole: ZIO[R, String, THole] = UniqueIdentifier.make


  val traitA = "TraitA"
  val traitB = "TraitB"
  val traitC = "TraitC"
  val traitD = "TraitD"
  val traitE = "TraitE"

  def traitType(t: String): WrapExpr = WrapExpr.OfExpr(ArExpr(ExprConstructor.TraitType(t), Vector()))
  def genTraitType: Gen[Any, WrapExpr] = Gen(Stream(traitA, traitB, traitC, traitD, traitE).map { t => Some(Sample(traitType(t), Stream.empty)) })


  private def impliesFalse(p: WrapExpr): WrapExpr =
    wrapExpr(ExprConstructor.FunctionType, (p, wrapExpr(ExprConstructor.NeverType, EmptyTuple)))


  override protected def isSubClass(classA: TClass, aArgs: Seq[WrapExpr], classB: TClass, bArgs: Seq[WrapExpr]): ZIO[R, String, SubClassResult] =
    IO.succeed(SubClassResult.NotSubClassProof(wrapExpr(ExprConstructor.AssumeErasedValue,
      impliesFalse(
        wrapExpr(ExprConstructor.SubtypeWitnessType,
          (
            WrapExpr.OfExpr(ArExpr(ExprConstructor.ClassType(classA), aArgs.toVector)),
            WrapExpr.OfExpr(ArExpr(ExprConstructor.ClassType(classB), bArgs.toVector))
          )
        )
      )
    )))

  private val subTraits: Seq[(String, String)] = Seq(
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

  override protected def isSubTrait(traitA: TTrait, aArgs: Seq[WrapExpr], traitB: TTrait, bArgs: Seq[WrapExpr]): ZIO[R, String, SubClassResult] =
    if(checkSubTraits(traitA, traitB)) {
      IO.succeed(SubClassResult.SubClassProof(wrapExpr(ExprConstructor.AssumeErasedValue,
        wrapExpr(ExprConstructor.SubtypeWitnessType,
          (
            WrapExpr.OfExpr(ArExpr(ExprConstructor.TraitType(traitA), aArgs.toVector)),
            WrapExpr.OfExpr(ArExpr(ExprConstructor.TraitType(traitB), bArgs.toVector))
          )
        )
      )))
    }
    else {
      IO.succeed(SubClassResult.NotSubClassProof(wrapExpr(ExprConstructor.AssumeErasedValue,
        impliesFalse(
          wrapExpr(ExprConstructor.SubtypeWitnessType,
            (
              WrapExpr.OfExpr(ArExpr(ExprConstructor.TraitType(traitA), aArgs.toVector)),
              WrapExpr.OfExpr(ArExpr(ExprConstructor.TraitType(traitB), bArgs.toVector))
            )
          )
        )
      )))
    }

  override protected def classImplementsTrait(classA: TClass, aArgs: Seq[WrapExpr], traitB: TTrait, bArgs: Seq[WrapExpr]): ZIO[R, String, SubClassResult] =
    IO.succeed(SubClassResult.NotSubClassProof(wrapExpr(ExprConstructor.AssumeErasedValue,
      impliesFalse(
        wrapExpr(ExprConstructor.SubtypeWitnessType,
          (
            WrapExpr.OfExpr(ArExpr(ExprConstructor.ClassType(classA), aArgs.toVector)),
            WrapExpr.OfExpr(ArExpr(ExprConstructor.TraitType(traitB), bArgs.toVector))
          )
        )
      )
    )))

  override protected def dataCtorImplementsTrait(dataCtorA: TDataConstructor, aArgs: Seq[WrapExpr], traitB: TTrait, bArgs: Seq[WrapExpr]): ZIO[R, String, SubClassResult] =
    IO.succeed(SubClassResult.NotSubClassProof(wrapExpr(ExprConstructor.AssumeErasedValue,
      impliesFalse(
        wrapExpr(ExprConstructor.SubtypeWitnessType,
          (
            WrapExpr.OfExpr(ArExpr(ExprConstructor.DataConstructorType(dataCtorA), aArgs.toVector)),
            WrapExpr.OfExpr(ArExpr(ExprConstructor.TraitType(traitB), bArgs.toVector))
          )
        )
      )
    )))


  override protected def traitRelations(arTrait: String): ZIO[R, String, Seq[ExprRelation]] = IO.succeed(Seq.empty)

  override protected def classRelations(arClass: String): ZIO[R, String, Seq[ExprRelation]] = IO.succeed(Seq.empty)

  override protected def dataConstructorRelations(dataConstructor: String): ZIO[R, String, Seq[ExprRelation]] = IO.succeed(Seq.empty)

  override protected def functionRelations(function: String): ZIO[R, String, Seq[ExprRelation]] =
    function match {
      case "natLessThan" => IO.succeed(Seq(ExprRelation.SyntacticEquality, ExprRelation.SyntacticEquality))
      case _ => IO.fail(s"Unknown function: $function")
    }

  override protected def methodRelations(method: String): ZIO[R, String, Seq[ExprRelation]] = IO.succeed(Seq.empty)

  override protected def classConstructorRelations(classCtor: String): ZIO[R, String, Seq[ExprRelation]] = IO.succeed(Seq.empty)

  override protected val natLessThanFunction: String = "natLessThan"
  override protected val boolType: WrapExpr = wrapExpr(ExprConstructor.ClassType("Ar.Bool"), Vector())

  override protected def invalidExpr: ZIO[R, String, Nothing] = IO.fail("Invalid expression")
  override protected def invalidPredicateExpr: ZIO[R, String, Nothing] = IO.fail("Invalid predicate expression")
  override protected def couldNotResolveImplicit: ZIO[R, String, Nothing] = IO.fail("Could not resolve implicit")

  override protected val evaluator: Evaluator[R, String] { val exprContext: TestResolver.this.exprContext.type } =
    new TestEvaluator[R, String]
}
