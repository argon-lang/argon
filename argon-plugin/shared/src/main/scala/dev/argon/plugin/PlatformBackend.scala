package dev.argon.plugin

import dev.argon.compiler.backend.{BackendBase, ExternHandler}

trait PlatformBackend extends BackendBase with Platform {
  override type TExternHandler <: ExternHandler {
    type ExternFunction = PlatformBackend.this.ExternFunction
    type ExternMethod = PlatformBackend.this.ExternMethod
  }
}
