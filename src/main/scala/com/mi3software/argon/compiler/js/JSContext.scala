package com.mi3software.argon.compiler.js

import com.mi3software.argon.compiler._
import com.mi3software.argon.util.Compilation
import scalaz.Monad

final class JSContext extends ContextComp[StandardCompilation] {

  override type TTraitMetadata = JSMetadata.Trait
  override type TClassMetadata = JSMetadata.Class

  override val invalidTraitMetadata: JSMetadata.Trait = JSMetadata.Trait.Invalid
  override val invalidClassMetadata: JSMetadata.Class = JSMetadata.Class.Invalid



  override type Comp[+T] = StandardCompilation[T]

  override val compMonadInstance: Monad[StandardCompilation] = StandardCompilation.monadInstance
  override val compCompilationInstance: Compilation[StandardCompilation] = StandardCompilation.compilationInstance

  override val typeSystem: ArgonTypeSystem[this.type] = new ArgonTypeSystem[this.type]
  override val moduleLoaders: Vector[ModuleLoader] = Vector(ArgonModuleLoader)
}
