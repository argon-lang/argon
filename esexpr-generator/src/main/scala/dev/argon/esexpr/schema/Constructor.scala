package dev.argon.esexpr.schema

import dev.argon.esexpr.*

final case class Constructor
(
  name: String,
  @keyword("inline") inlineValue: Option[Boolean],
  parameters: Parameter*
) derives ESExprCodec
