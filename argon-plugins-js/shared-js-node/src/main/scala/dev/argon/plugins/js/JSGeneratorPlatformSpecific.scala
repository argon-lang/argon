package dev.argon.plugins.js

import zio.*

trait JSGeneratorPlatformSpecific {
  protected def generateImpl(value: JSValue): UIO[String] =
    ZIO.succeed {
      Astring.generate(value)
    }
}
