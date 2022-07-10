package dev.argon.plugins.js

import zio.*

object JSGenerator extends JSGeneratorPlatformSpecific {
  def generate[A: JSValueEncoder](a: A): UIO[String] =
    generateImpl(summon[JSValueEncoder[A]].toJSValue(a))
}
