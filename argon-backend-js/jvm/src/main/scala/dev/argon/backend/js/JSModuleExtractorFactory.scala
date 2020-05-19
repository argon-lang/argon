package dev.argon.backend.js

import zio.{URIO, ZIO}
import zio.blocking.Blocking

object JSModuleExtractorFactory {
  def make: URIO[Blocking, JSModuleExtractor] =
    ZIO.access[Blocking] { env => JSModuleExtractorImpl(env.get) }
}
