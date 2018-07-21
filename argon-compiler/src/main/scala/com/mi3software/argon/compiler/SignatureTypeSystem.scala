package com.mi3software.argon.compiler

final class SignatureTypeSystem[TContext <: Context] extends TypeSystem {

  override type TType = Option[TypeBaseConcrete[this.type]]
  override type TTraitInfo = ArTrait[TContext]
  override type TClassInfo = ArClass[TContext]
  override type TDataConstructorInfo = DataConstructor[TContext]

  override type TTupleElementType = None.type
  override type TFunctionArgumentType = None.type
  override type TFunctionResultType = None.type

  override def fromTypeBaseConcrete(typeBase: TypeBaseConcrete[SignatureTypeSystem.this.type]): Option[TypeBaseConcrete[SignatureTypeSystem.this.type]] =
    Some(typeBase)
}
