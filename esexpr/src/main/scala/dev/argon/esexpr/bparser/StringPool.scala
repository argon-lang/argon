package dev.argon.esexpr.bparser

import dev.argon.esexpr.ESExprCodec

final case class StringPool(
  symbols: String*,
) derives ESExprCodec {
  lazy val reverseLookup: Map[String, Int] = symbols.zipWithIndex.toMap 
}
