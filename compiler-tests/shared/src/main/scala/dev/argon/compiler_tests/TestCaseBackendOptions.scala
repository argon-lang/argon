package dev.argon.compiler_tests

import dev.argon.backend.options.OptionValue

object TestCaseBackendOptions {
  def tubeOptionsProvider: BackendOptionsProvider =
    BackendOptionsProvider(
      "js" -> Map(),
    )
  
  def codeGenOptionsProvider: BackendOptionsProvider =
    BackendOptionsProvider(
      "js" -> Map(),
    )
}
