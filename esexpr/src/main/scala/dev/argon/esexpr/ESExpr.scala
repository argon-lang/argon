package dev.argon.esexpr

import zio.Chunk

sealed trait ESExpr derives CanEqual

object ESExpr {
  final case class Constructed(constructor: String, kwargs: Map[String, ESExpr], args: Seq[ESExpr]) extends ESExpr
  final case class Bool(b: Boolean) extends ESExpr
  final case class Int(n: BigInt) extends ESExpr
  final case class Str(s: String) extends ESExpr
  final case class Binary(b: Chunk[Byte]) extends ESExpr
  final case class Float32(f: Float) extends ESExpr
  final case class Float64(d: Double) extends ESExpr
  case object Null extends ESExpr
}
