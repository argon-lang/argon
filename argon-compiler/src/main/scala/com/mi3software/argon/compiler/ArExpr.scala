package com.mi3software.argon.compiler

import com.mi3software.argon.compiler.core._
import com.mi3software.argon.compiler.types._

trait ArExprContext extends VariableContext {

  import typeSystem._

  trait ArExpr

  final case class ClassConstructorCall(classType: ClassType, classCtor: AbsRef[context.type, ClassConstructor], args: Vector[TType]) extends ArExpr
  final case class TupleElement(value: TType)
  final case class CreateTuple(values: Vector[TupleElement]) extends ArExpr
  final case class DataConstructorCall(dataCtorInstanceType: DataConstructorType, args: Vector[TType]) extends ArExpr
  final case class FunctionCall(function: AbsRef[context.type, ArFunc], args: Vector[ArExpr], returnType: TType) extends ArExpr
  final case class IfElse(condition: ArExpr, ifBody: ArExpr, elseBody: ArExpr) extends ArExpr
  final case class LetBinding(variable: Variable[VariableDescriptor], value: ArExpr, next: ArExpr) extends ArExpr
  final case class LoadConstantBool(value: Boolean) extends ArExpr
  final case class LoadConstantInt(value: BigInt) extends ArExpr
  final case class LoadConstantString(value: String) extends ArExpr
  final case class LoadLambda(argVariable: Variable[VariableDescriptor], body: ArExpr) extends ArExpr
  final case class LoadTypeValue(value: TType) extends ArExpr
  final case class LoadVariable(variable: Variable[VariableLikeDescriptor]) extends ArExpr
  final case class MethodCall(method: AbsRef[context.type, ArMethod], instance: ArExpr, args: Vector[ArExpr]) extends ArExpr
  final case class Sequence(first: ArExpr, second: ArExpr) extends ArExpr
  final case class StoreVariable(variable: Variable[VariableLikeDescriptor]) extends ArExpr
  
  
}


