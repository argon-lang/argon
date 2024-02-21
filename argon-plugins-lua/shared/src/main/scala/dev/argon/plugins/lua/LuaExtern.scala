package dev.argon.plugins.lua

import dev.argon.esexpr.{ESExpr, ESExprCodec, ESExprTag}
import dev.argon.plugin.Extern

object LuaExtern extends Extern {
  override type Implementation = LuaExternImplementation
  override def implementationCodec: ESExprCodec[Implementation] = summon[ESExprCodec[Implementation]]

  override type Reference = Nothing
  override def referenceCodec: ESExprCodec[Reference] =
    new ESExprCodec[Nothing]:
      override lazy val tags: Set[ESExprTag] = Set.empty

      override def encode(value: Nothing): ESExpr = value
      override def decode(expr: ESExpr): Either[String, Nothing] =
        Left("Cannot decode references")
    end new
}
