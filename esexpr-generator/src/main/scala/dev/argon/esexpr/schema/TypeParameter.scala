package dev.argon.esexpr.schema

import dev.argon.esexpr.*

final case class TypeParameter(
  name: String,
  @keyword tuple: Option[String],
  @keyword `enum`: Option[String],
) derives ESExprCodec, CanEqual
