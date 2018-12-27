package com.mi3software.argon.compiler.types

import com.mi3software.argon.compiler.Compilation
import scalaz.Scalaz._

trait TypeSystemConverter[TS1 <: TypeSystem, TS2 <: TypeSystem] {
  def convertType[TComp[+_] : Compilation]
  (t: TS1#TType)
  : TComp[TS2#TType]
}

