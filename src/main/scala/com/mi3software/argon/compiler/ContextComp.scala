package com.mi3software.argon.compiler

trait ContextComp[TComp[+_]] extends Context {
  override type Comp[+A] = TComp[A]
  override val withCompType: this.type = this
}
