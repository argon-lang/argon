package dev.argon.backend.js

import zio.{IO, Task}

object JSModuleExtractorImpl extends JSModuleExtractor {

  @SuppressWarnings(Array("dev.argon.warts.ZioEffect"))
  private[js] def exportedFunctions(module: String): Task[Map[String, String]] =
    IO.effect {
      dev.argon.js_module_extractor.JSModuleExtractor.usingScalaMap(module)
    }

}
