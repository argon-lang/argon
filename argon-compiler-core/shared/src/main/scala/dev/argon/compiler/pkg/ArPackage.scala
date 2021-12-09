package dev.argon.compiler.pkg

import dev.argon.compiler.*
import dev.argon.compiler.module.*

trait ArPackageC extends UsingContext {
  val packageName: PackageName
  def module(path: ModulePath): Comp[ArModule]
}
