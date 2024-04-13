package dev.argon.prover.prolog

import cats.data.OptionT
import dev.argon.util.{*, given}
import dev.argon.prover.*
import dev.argon.prover.prolog.PrologContext
import zio.*
import zio.stream.{Stream, ZStream}
import zio.test.Assertion
import cats.*
import cats.implicits.given
import zio.interop.catz.core.*

abstract class SimplePrologContext[Constructor](using CanEqual[Constructor, Constructor])
    extends PrologContext[VariableProvider, Nothing] with TestProverContextBase[Constructor] {
  override val syntax: SimpleProverSyntax[Constructor]
  import syntax.*

  override protected def intrinsicPredicate(predicate: Expr, model: Model, solveState: SolveState): ZStream[VariableProvider, Nothing, ProofResult.Definitive] =
    ZStream.empty

}
