package dev.argon.compiler_tests

import dev.argon.compiler_tests.BackendOptionsProvider.OptionsFactory
import dev.argon.backend.platforms.js.JSBackend

object TestCaseBackendOptions {
  def provider: BackendOptionsProvider =
    BackendOptionsProvider(
      OptionsFactory[JSBackend[TestError]](_.JSOptions(
        externs = Seq(),
      )),
    )
}
