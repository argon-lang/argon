package dev.argon.backend.js

import zio.{IO, UIO}

object JSModuleExtractorFactory {
  def make: UIO[JSModuleExtractor] = IO.succeed(JSModuleExtractorImpl)
}
