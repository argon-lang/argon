package dev.argon.backend.scalaApi

import nobleidl.core.JavaAdapter

trait TestExecutionFailureCompanionPlatformSpecific {
  def javaAdapter(): JavaAdapter[Throwable, Throwable] =
    JavaAdapter.identity
}
