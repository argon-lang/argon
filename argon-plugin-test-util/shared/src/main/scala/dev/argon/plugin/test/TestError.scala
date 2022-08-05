package dev.argon.plugin.test

enum TestError {
  case ErrorLoadingBuildConfig(error: String)
  case ExecutionError(error: Throwable)
}
