package dev.argon.prover


import zio.*
import zio.stream.*

trait ProverContext[R, E] {
  val syntax: ProverSyntax
  import syntax.*

  type Error = Either[E, ProofResult.No]

  type ProofAtom

  // Relation is for comparing expressions as arguments to a constructor (ex: A <: B, A =:= B, A = B)
  type TRelation
  type TConstraints

  type Model = Map[syntax.TVariable, TConstraints]

  sealed trait ProofResult

  object ProofResult {
    final case class Yes(proof: Proof[ProofAtom], model: Model) extends ProofResult
    final case class No(notProof: Proof[ProofAtom], model: Model) extends ProofResult
    case object Unknown extends ProofResult

    given CanEqual[Unknown.type, ProofResult] = CanEqual.canEqualAny
    given CanEqual[ProofResult, Unknown.type] = CanEqual.canEqualAny
  }


  protected def assertions: Seq[ZIO[R, E, TVariable] => ZIO[R, E, (Proof[ProofAtom], Predicate)]]
  protected def normalize(expr: Value, substitutions: Model, fuel: Int): ZIO[R, E, Expr]
  protected def otherForEquivalenceRelation(constraints: TConstraints): Option[syntax.Expr]

  final def check(goal: Predicate, fuel: Int): ZIO[R, E, ProofResult] = check(goal, Map.empty, fuel)

  def check(goal: Predicate, model: Model, fuel: Int): ZIO[R, E, ProofResult]


  protected def normalizeExpr(e: Expr, substitutions: Model, fuel: Int): ZIO[R, E, Expr] =
    if fuel < 0 then
      ZIO.succeed(e)
    else
      e match {
        case value: Value => normalize(value, substitutions, fuel)
        case Variable(variable) =>
          substitutions.get(variable).flatMap(otherForEquivalenceRelation) match {
            case Some(value) => normalizeExpr(value, substitutions, fuel - 1)
            case None => ZIO.succeed(e)
          }
      }

  protected def normalizePredicateFunction(pf: PredicateFunction, model: Model, fuel: Int): ZIO[R, E, PredicateFunction] =
    for
      args2 <- ZIO.foreach(pf.args)(normalizeExpr(_, model, fuel))
    yield PredicateFunction(pf.function, args2)
  
}
