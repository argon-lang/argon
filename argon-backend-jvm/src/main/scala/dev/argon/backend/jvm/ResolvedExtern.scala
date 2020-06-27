package dev.argon.backend.jvm

import cats.kernel.Semigroup
import org.objectweb.asm.tree.MethodNode

sealed trait ResolvedExtern
object ResolvedExtern {
  final case class Method(node: MethodNode) extends ResolvedExtern
  case object Ambiguous extends ResolvedExtern

  implicit val resolvedExternSemigroupInstance: Semigroup[ResolvedExtern] = new Semigroup[ResolvedExtern] {
    override def combine(x: ResolvedExtern, y: ResolvedExtern): ResolvedExtern = Ambiguous
  }
}
