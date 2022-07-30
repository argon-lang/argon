package dev.argon.plugins.source

import dev.argon.compiler.UsingContext

trait ExternLoader extends UsingContext {
  def externMethod(id: String): Comp[context.ExternMethodImplementation]
}
