package dev.argon.backend.js

import dev.argon.options.SingleFile

final case class JSInjectCode(before: Option[SingleFile], after: Option[SingleFile])
