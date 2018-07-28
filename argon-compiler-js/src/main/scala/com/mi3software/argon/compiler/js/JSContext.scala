package com.mi3software.argon.compiler.js

import com.mi3software.argon.compiler.{Compilation, _}

final class JSContext extends ContextComp[StandardCompilation.StandardCompilationType] {

  override type TTraitMetadata = JSMetadata.Trait
  override type TClassMetadata = JSMetadata.Class

  override val invalidTraitMetadata: JSMetadata.Trait = JSMetadata.Trait.Invalid
  override val invalidClassMetadata: JSMetadata.Class = JSMetadata.Class.Invalid



  override type Comp[+T] = StandardCompilation.StandardCompilationType[T]

  override val compCompilationInstance: Compilation[Comp] = StandardCompilation.standardCompilationInstance

  override val typeSystem: ArgonTypeSystem[this.type] = new ArgonTypeSystem[this.type]
  override val moduleLoaders: Vector[ModuleLoader] = Vector(ArgonModuleLoader)
}
