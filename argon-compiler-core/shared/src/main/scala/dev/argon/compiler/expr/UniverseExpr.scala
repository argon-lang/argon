package dev.argon.compiler.expr

sealed trait UniverseExpr
final case class FixedUniverse(u: BigInt) extends UniverseExpr
final case class AbstractUniverse() extends UniverseExpr
final case class LargestUniverse(a: UniverseExpr, b: UniverseExpr) extends UniverseExpr
final case class NextLargestUniverse(a: UniverseExpr) extends UniverseExpr
final case class PreviousUniverse(a: UniverseExpr) extends UniverseExpr
