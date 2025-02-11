package dev.argon.backend.platforms.js

import dev.argon.backend.BackendException

final case class JavaScriptException(message: String) extends BackendException(message)
