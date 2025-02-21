package dev.argon.compiler_tests

import dev.argon.backend.options.OptionValue

object TestCaseBackendOptions {
  def provider: BackendOptionsProvider =
    BackendOptionsProvider(
      "js" -> Map(),
    )
}
