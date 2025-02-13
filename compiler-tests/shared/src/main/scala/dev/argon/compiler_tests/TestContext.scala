package dev.argon.compiler_tests

import dev.argon.compiler.*

type TestContext = Context {
  type Env = ErrorLog
  type Error = TestError
}
