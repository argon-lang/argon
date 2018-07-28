package com.mi3software.argon.compiler

import scalaz.Monad

trait TypeSystemConverter[TS1 <: TypeSystem, TS2 <: TypeSystem] {
  def convertType[TComp[+_] : Monad : Compilation]
  (t: TS1#TType)
  : TComp[TS2#TType]
}
