package dev.argon.esexpr.schema

import dev.argon.esexpr.ESExprCodec

final case class EnumCase(
  name: String,
  members: Member*
) derives ESExprCodec, CanEqual
