package dev.argon.backend.js

import zio.{IO, Task}

object JSModuleExtractor {

  @SuppressWarnings(Array("dev.argon.warts.ZioEffect"))
  def exportedFunctions(module: String): Task[Map[String, String]] =
    IO.effect {
      dev.argon.js_module_extractor.JSModuleExtractor.usingScalaMap(module)
    }

}
