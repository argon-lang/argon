package dev.argon.backend.js

import zio.{IO, UIO}

object JSModuleExtractorFactory {
  type ModuleExtractorEnv = Any
  def make: UIO[JSModuleExtractor] = IO.succeed(JSModuleExtractorImpl)
}
