package dev.argon.compiler_tests

import dev.argon.backend.backends.js.JSBackend
import dev.argon.compiler_tests.BackendOptionsProvider.OptionsFactory

object TestCaseBackendOptions {
  def provider: BackendOptionsProvider =
    BackendOptionsProvider(
      OptionsFactory[JSBackend[TestError]](_.JSOptions(
        externs = Seq(),
      )),
    )
}
