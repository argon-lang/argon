package dev.argon.backend.js

import zio.Task

trait JSModuleExtractor {
  private[js] def exportedFunctions(module: String): Task[Map[String, String]]
}
