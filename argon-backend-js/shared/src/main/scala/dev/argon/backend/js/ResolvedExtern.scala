package dev.argon.backend.js

import cats.kernel.Semigroup

sealed trait ResolvedExtern
object ResolvedExtern {
  final case class Function(code: String) extends ResolvedExtern
  case object Ambiguous extends ResolvedExtern

  implicit val resolvedExternSemigroupInstance: Semigroup[ResolvedExtern] = new Semigroup[ResolvedExtern] {
    override def combine(x: ResolvedExtern, y: ResolvedExtern): ResolvedExtern = Ambiguous
  }
}
