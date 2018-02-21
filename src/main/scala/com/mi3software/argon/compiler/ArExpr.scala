package com.mi3software.argon.compiler

trait ArExprTypes {
  type TS <: TypeSystem
  type TExpr
  type TFunction
  type TMethod
  type TClassConstructor
}

trait ArExpr[Types <: ArExprTypes]

final case class InvalidExpression[Types <: ArExprTypes]() extends ArExpr[Types]

final case class ClassConstructorCall[Types <: ArExprTypes](classType: ClassType[Types#TS], classCtor: Types#TClassConstructor, args: Vector[Types#TExpr]) extends ArExpr[Types]
final case class TupleElement[Types <: ArExprTypes](value: Types#TExpr)
final case class CreateTuple[Types <: ArExprTypes](values: Vector[TupleElement[Types]]) extends ArExpr[Types]
final case class DataConstructorCall[Types <: ArExprTypes](dataCtorInstanceType: DataConstructorType[Types#TS], args: Vector[Types#TExpr]) extends ArExpr[Types]
final case class FunctionCall[Types <: ArExprTypes](function: Types#TFunction, args: Vector[Types#TExpr], returnType: Types#TS#TType) extends ArExpr[Types]
final case class IfElse[Types <: ArExprTypes](condition: Types#TExpr, ifBody: Types#TExpr, elseBody: Types#TExpr) extends ArExpr[Types]
final case class LetBinding[Types <: ArExprTypes](variable: Variable[Types#TS, VariableDescriptor], value: Types#TExpr, next: Types#TExpr) extends ArExpr[Types]
final case class LoadConstantBool[Types <: ArExprTypes](value: Boolean) extends ArExpr[Types]
final case class LoadConstantInt[Types <: ArExprTypes](value: BigInt) extends ArExpr[Types]
final case class LoadConstantString[Types <: ArExprTypes](value: String) extends ArExpr[Types]
final case class LoadLambda[Types <: ArExprTypes](argVariable: Variable[Types#TS, VariableDescriptor], body: Types#TExpr) extends ArExpr[Types]
final case class LoadTypeValue[Types <: ArExprTypes](value: Types#TS#TType) extends ArExpr[Types]
final case class LoadVariable[Types <: ArExprTypes](variable: Variable[Types#TS, VariableLikeDescriptor]) extends ArExpr[Types]
final case class MethodCall[Types <: ArExprTypes](method: Types#TMethod, instance: Types#TExpr, args: Vector[Types#TExpr]) extends ArExpr[Types]
final case class Sequence[Types <: ArExprTypes](first: Types#TExpr, second: Types#TExpr) extends ArExpr[Types]
final case class StoreVariable[Types <: ArExprTypes](variable: Variable[Types#TS, VariableLikeDescriptor]) extends ArExpr[Types]

