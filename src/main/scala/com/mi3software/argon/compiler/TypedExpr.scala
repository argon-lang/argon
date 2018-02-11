package com.mi3software.argon.compiler

trait TypedExprTypes {
  type TS <: TypeSystem
  type TFunction
  type TMethod
  type TClassConstructor
}

trait TypedExpr[Types <: TypedExprTypes]

final case class ClassConstructorCall[Types <: TypedExprTypes](classType: ClassType[Types#TS], classCtor: Types#TClassConstructor, args: Vector[TypedExpr[Types]]) extends TypedExpr[Types]
final case class TupleElement[Types <: TypedExprTypes](value: TypedExpr[Types])
final case class CreateTuple[Types <: TypedExprTypes](values: Vector[TupleElement[Types]]) extends TypedExpr[Types]
final case class DataConstructorCall[Types <: TypedExprTypes](dataCtorInstanceType: DataConstructorType[Types#TS], args: Vector[TypedExpr[Types]]) extends TypedExpr[Types]
final case class FunctionCall[Types <: TypedExprTypes](function: Types#TFunction, args: Vector[TypedExpr[Types]], returnType: Types#TS#TType) extends TypedExpr[Types]
final case class IfElse[Types <: TypedExprTypes](condition: TypedExpr[Types], ifBody: TypedExpr[Types], elseBody: TypedExpr[Types]) extends TypedExpr[Types]
final case class LetBinding[Types <: TypedExprTypes](variable: Variable[Types#TS, VariableDescriptor], value: TypedExpr[Types], next: TypedExpr[Types]) extends TypedExpr[Types]
final case class LoadConstantBool[Types <: TypedExprTypes](value: Boolean) extends TypedExpr[Types]
final case class LoadConstantInt[Types <: TypedExprTypes](value: BigInt) extends TypedExpr[Types]
final case class LoadConstantString[Types <: TypedExprTypes](value: String) extends TypedExpr[Types]
final case class LoadLambda[Types <: TypedExprTypes](argVariable: Variable[Types#TS, VariableDescriptor], body: TypedExpr[Types]) extends TypedExpr[Types]
final case class LoadTypeValue[Types <: TypedExprTypes](value: Types#TS#TType) extends TypedExpr[Types]
final case class LoadVariable[Types <: TypedExprTypes](variable: Variable[Types#TS, VariableLikeDescriptor]) extends TypedExpr[Types]
final case class MethodCall[Types <: TypedExprTypes](method: Types#TMethod, instance: TypedExpr[Types], args: Vector[TypedExpr[Types]]) extends TypedExpr[Types]
final case class Sequence[Types <: TypedExprTypes](first: TypedExpr[Types], second: TypedExpr[Types]) extends TypedExpr[Types]
final case class StoreVariable[Types <: TypedExprTypes](variable: Variable[Types#TS, VariableLikeDescriptor]) extends TypedExpr[Types]

