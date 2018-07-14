package com.mi3software.argon.compiler

final class SignatureTypeSystem[TContext <: Context] extends TypeSystem {

  override type TType = TypeBase[this.type]
  override type TTraitInfo = ArTrait[TContext]
  override type TClassInfo = ArClass[TContext]
  override type TDataConstructorInfo = DataConstructor[TContext]

  override def fromTypeBase(typeBase: TypeBase[SignatureTypeSystem.this.type]): TypeBase[SignatureTypeSystem.this.type] = typeBase

}
