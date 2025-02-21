package dev.argon.backend.options

import dev.argon.backend.{scalaApi, BackendException}

final case class OptionParseFailure(failure: scalaApi.options.OptionParseFailure) extends BackendException(cause = failure)
