package dev.argon.plugins.js

import zio.*

object JSGenerator extends JSGeneratorPlatformSpecific {
  def generate[A: JSValueCodec](a: A): UIO[String] =
    generateImpl(summon[JSValueCodec[A]].toJSValue(a))
}
