package dev.argon.build.testrunner.js

import java.io.IOException

import dev.argon.compiler.loaders.ResourceIndicator
import zio.IO

object JSModuleLoad {

  trait Service {
    def loadJSForArgonModule(id: ResourceIndicator): IO[IOException, (String, String)]
  }

}
