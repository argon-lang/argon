package com.mi3software.argon.compiler

trait Context {

  type TFunctionImplementation
  type TMethodImplementation
  type TConstructorImplementation
  type TClassConstructorImplementation

  type TFunctionMetadata
  type TMethodMetadata
  type TTraitMetadata
  type TClassMetadata
  type TConstructorMetadata
  type TClassConstructorMetadata

  type Comp[+_]

  val typeSystem: TypeSystem

}
