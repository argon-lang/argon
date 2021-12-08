package dev.argon.compiler.pkg

import dev.argon.compiler._
import dev.argon.compiler.module._

trait ArPackageC extends UsingContext {
  val packageName: PackageName
  def module(path: ModulePath): Comp[ArModule]
}

