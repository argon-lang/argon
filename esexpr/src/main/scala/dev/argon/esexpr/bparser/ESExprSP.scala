package dev.argon.esexpr.bparser

import zio.Chunk

enum ESExprSP {
  case Constructed(constructor: BigInt, kwargs: Map[BigInt, ESExprSP], args: Seq[ESExprSP])
  case Bool(b: Boolean)
  case Int(n: BigInt)
  case Str(s: String)
  case StrPooled(s: BigInt)
  case Binary(b: Chunk[Byte])
  case Float32(f: Float)
  case Float64(d: Double)
  case Null
}
