package com.mi3software.argon.compiler.js

import com.mi3software.argon.compiler._
import com.mi3software.argon.util.Compilation
import scalaz.Monad

class JSContext extends Context {

  override type TTraitMetadata = JSMetadata.Trait
  override type TClassMetadata = JSMetadata.Class

  override val invalidClassMetadata: TClassMetadata = JSMetadata.Class.Invalid



  override type Comp[+T] = StandardCompilation[T]

  override val typeSystem: ArgonTypeSystem[this.type] = new ArgonTypeSystem[this.type]
  override val compMonadInstance: Monad[StandardCompilation] = StandardCompilation.monadInstance
  override val compCompilationInstance: Compilation[StandardCompilation] = StandardCompilation.compilationInstance

}
