package dev.argon.esexpr

import zio.Chunk

enum ESExpr derives CanEqual {
  case Constructed(constructor: String, kwargs: Map[String, ESExpr], args: Seq[ESExpr])
  case Bool(b: Boolean)
  case Int(n: BigInt)
  case Str(s: String)
  case Binary(b: Chunk[Byte])
  case Float32(f: Float)
  case Float64(d: Double)
  case Null
}
