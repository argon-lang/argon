package com.mi3software.argon.compiler

import scalaz.{-\/, \/}

final class HoleTypeSystem[TContext <: Context] extends TypeSystem {
  override type TType = TypeBase[this.type] \/ TypeHole

  override def fromTypeBase(typeBase: TypeBase[this.type]): TypeBase[this.type] \/ TypeHole =
    -\/(typeBase)

  override type TTraitInfo = (ArTrait[TContext], Vector[TType], ArTrait.ResultInfo[this.type])
  override type TClassInfo = (ArClass[TContext], Vector[TType], ArClass.ResultInfo[this.type])
  override type TDataConstructorInfo = (DataConstructor[TContext], Vector[TType], DataConstructor.ResultInfo[this.type])
}

final case class TypeHole(id: Int)
