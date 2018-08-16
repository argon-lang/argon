package com.mi3software.argon.compiler

import scalaz._
import Scalaz._

trait TypeSystemConverter[TS1 <: TypeSystem, TS2 <: TypeSystem] {
  def convertType[TComp[+_] : Compilation]
  (t: TS1#TType)
  : TComp[TS2#TType]
}

trait TypeSystemConverterTotal[TS1 <: TypeSystem, TS2 <: TypeSystem] extends TypeSystemConverter[TS1, TS2] {
  final override def convertType[TComp[+ _] : Compilation](t: TS1#TType): TComp[TS2#TType] = convertTypeTotal(t).point[TComp]

  def convertTypeTotal(t: TS1#TType): TS2#TType
}
