package dev.argon.plugins.js

import zio.*

trait GenerateTestsHelper {
  def generate[A: JSValueCodec](a: A): UIO[String] =
    ZIO.scoped(JSContext.make.flatMap { context => context.generate(a) })
}
