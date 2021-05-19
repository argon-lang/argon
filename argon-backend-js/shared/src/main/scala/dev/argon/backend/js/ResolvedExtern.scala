package dev.argon.backend.js

import cats.kernel.CommutativeSemigroup

sealed trait ResolvedExtern
object ResolvedExtern {
  final case class Function(code: String) extends ResolvedExtern
  case object Ambiguous extends ResolvedExtern

  implicit val resolvedExternSemigroupInstance: CommutativeSemigroup[ResolvedExtern] = new CommutativeSemigroup[ResolvedExtern] {
    override def combine(x: ResolvedExtern, y: ResolvedExtern): ResolvedExtern = Ambiguous
  }
}
