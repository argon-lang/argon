package dev.argon.expr

import dev.argon.util.UniqueIdentifier
import dev.argon.prover.Proof
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


  private val type0: WrapExpr =
    WrapExpr.OfExpr(ArExpr(
      ExprConstructor.TypeN,
      WrapExpr.OfExpr(ArExpr(
        ExprConstructor.LoadConstantInt(0),
        EmptyTuple
      ))
    ))


  override protected def typeOfClass(classObj: String, args: Seq[WrapExpr]): ZIO[R, String, WrapExpr] =
    ZIO.succeed(type0)

  override protected def typeOfTrait(traitObj: String, args: Seq[WrapExpr]): ZIO[R, String, WrapExpr] =
    ZIO.succeed(type0)


  protected override def isSubClass
  (
    prologContext: IRPrologContext,
    classA: TClass,
    aArgs: Seq[ExprProverSyntax.Expr],
    classB: TClass,
    bArgs: Seq[ExprProverSyntax.Expr],
    model: prologContext.Model,
    solveState: prologContext.SolveState,
  )
  : ZStream[R, Either[String, prologContext.ProofResult.No], prologContext.ProofResult.Yes] =
    ZStream.fail(Right(prologContext.ProofResult.No(
      Proof.Atomic(TCAtomicProof.ExprProof(wrapExpr(
        ExprConstructor.AssumeErasedValue,
        EmptyTuple,
      ))),
      model,
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

  protected def isSubTrait
  (
    prologContext: IRPrologContext,
    traitA: TTrait,
    aArgs: Seq[ExprProverSyntax.Expr],
    traitB: TTrait,
    bArgs: Seq[ExprProverSyntax.Expr],
    model: prologContext.Model,
    solveState: prologContext.SolveState,
  )
  : ZStream[R, Either[String, prologContext.ProofResult.No], prologContext.ProofResult.Yes] =
    if checkSubTraits(traitA, traitB) then
      ZStream.succeed(prologContext.ProofResult.Yes(
        Proof.Atomic(TCAtomicProof.ExprProof(wrapExpr(
          ExprConstructor.AssumeErasedValue,
          EmptyTuple,
        ))),
        model,
      ))
    else
      ZStream.fail(Right(prologContext.ProofResult.No(
        Proof.Atomic(TCAtomicProof.ExprProof(wrapExpr(
          ExprConstructor.AssumeErasedValue,
          EmptyTuple,
        ))),
        model,
      )))


  protected def classImplementsTrait
  (
    prologContext: IRPrologContext,
    classA: TClass,
    aArgs: Seq[ExprProverSyntax.Expr],
    traitB: TTrait,
    bArgs: Seq[ExprProverSyntax.Expr],
    model: prologContext.Model,
    solveState: prologContext.SolveState,
  )
  : ZStream[R, Either[String, prologContext.ProofResult.No], prologContext.ProofResult.Yes] =
    ZStream.fail(Right(prologContext.ProofResult.No(
      Proof.Atomic(TCAtomicProof.ExprProof(wrapExpr(
        ExprConstructor.AssumeErasedValue,
        EmptyTuple,
      ))),
      model,
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

  override protected def substituteVariables(vars: Map[String, exprContext.WrapExpr])(expr: exprContext.WrapExpr): exprContext.WrapExpr =
    expr

  protected override val natLessThanFunction: ZIO[R, String, String] = ZIO.succeed("natLessThan")
  protected override val boolType: ZIO[R, String, WrapExpr] =
    ZIO.succeed(wrapExpr(ExprConstructor.ClassType("Ar.Bool"), Vector()))

  protected override def invalidExpr: ZIO[R, String, Nothing] = ZIO.fail("Invalid expression")
  protected override def invalidPredicateExpr: ZIO[R, String, Nothing] = ZIO.fail("Invalid predicate expression")

  protected override lazy val evaluator: Evaluator[R, String] { val exprContext: TestResolver.this.exprContext.type } =
    new TestEvaluator[R, String]

}
