package dev.argon.compiler.expr

import cats.data.NonEmptyList
import dev.argon.compiler.core._

import scala.collection.immutable.Vector

sealed trait ArExpr[TContext, Wrap[+_]]

object ArExpr {
  final case class ClassConstructorCall[TContext <: Context with Singleton, Wrap[+_]]
  (
    classType: ClassType[TContext, Wrap],
    classCtor: AbsRef[TContext, ClassConstructor],
    args: Vector[Wrap[ArExpr[TContext, Wrap]]]
  ) extends ArExpr[TContext, Wrap]

  final case class DataConstructorCall[TContext <: Context with Singleton, Wrap[+_]]
  (
    dataCtorInstanceType: DataConstructorType[TContext, Wrap],
    args: Vector[Wrap[ArExpr[TContext, Wrap]]]
  ) extends ArExpr[TContext, Wrap]
  
  final case class EnsureExecuted[TContext, Wrap[+_]]
  (
    body: ArExprWrap[TContext, Wrap],
    ensuring: ArExprWrap[TContext, Wrap]
  ) extends ArExpr[TContext, Wrap]
  
  final case class FunctionCall[TContext <: Context with Singleton, Wrap[+_]]
  (
    function: AbsRef[TContext, ArFunc],
    args: Vector[Wrap[ArExpr[TContext, Wrap]]],
    returnType: ArExprWrap[TContext, Wrap]
  ) extends ArExpr[TContext, Wrap]

  final case class FunctionObjectCall[TContext, Wrap[+_]]
  (
    function: ArExprWrap[TContext, Wrap],
    arg: ArExprWrap[TContext, Wrap],
    returnType: ArExprWrap[TContext, Wrap]
  ) extends ArExpr[TContext, Wrap]
  
  final case class IfElse[TContext, Wrap[+_]]
  (
    condition: ArExprWrap[TContext, Wrap],
    ifBody: ArExprWrap[TContext, Wrap],
    elseBody: ArExprWrap[TContext, Wrap]
  ) extends ArExpr[TContext, Wrap]
  
  final case class LetBinding[TContext, Wrap[+_]]
  (
    variable: LocalVariable[TContext, Wrap],
    value: ArExprWrap[TContext, Wrap],
    next: ArExprWrap[TContext, Wrap]
  ) extends ArExpr[TContext, Wrap]
  
  final case class LoadConstantBool[TContext, Wrap[+_]]
  (
    value: Boolean,
    exprType: ArExprWrap[TContext, Wrap]
  ) extends ArExpr[TContext, Wrap]
  
  final case class LoadConstantInt[TContext, Wrap[+_]]
  (
    value: BigInt,
    exprType: ArExprWrap[TContext, Wrap]
  ) extends ArExpr[TContext, Wrap]
  
  final case class LoadConstantString[TContext, Wrap[+_]]
  (
    value: String,
    exprType: ArExprWrap[TContext, Wrap]
  ) extends ArExpr[TContext, Wrap]
  
  final case class LoadLambda[TContext, Wrap[+_]]
  (
    argVariable: LocalVariable[TContext, Wrap],
    body: ArExprWrap[TContext, Wrap]
  ) extends ArExpr[TContext, Wrap]
  
  final case class TupleElement[TContext, Wrap[+_]](value: ArExprWrap[TContext, Wrap])
  
  final case class LoadTuple[TContext, Wrap[+_]]
  (
    values: NonEmptyList[TupleElement[TContext, Wrap]]
  ) extends ArExpr[TContext, Wrap]
  
  final case class LoadTupleElement[TContext, Wrap[+_]]
  (
    tupleValue: ArExprWrap[TContext, Wrap],
    elemType: ArExprWrap[TContext, Wrap],
    index: Int
  ) extends ArExpr[TContext, Wrap]
  
  final case class LoadUnit[TContext, Wrap[+_]]
  (
    exprType: ArExprWrap[TContext, Wrap]
  ) extends ArExpr[TContext, Wrap]
  
  final case class LoadVariable[TContext, Wrap[+_]]
  (
    variable: Variable[TContext, Wrap]
  ) extends ArExpr[TContext, Wrap]
  
  final case class MethodCall[TContext <: Context with Singleton, Wrap[+_]]
  (
    method: AbsRef[TContext, ArMethod],
    instance: ArExprWrap[TContext, Wrap],
    args: Vector[Wrap[ArExpr[TContext, Wrap]]],
    returnType: ArExprWrap[TContext, Wrap]
  ) extends ArExpr[TContext, Wrap]

  final case class PatternMatch[TContext, Wrap[+_]]
  (
    expr: ArExprWrap[TContext, Wrap],
    cases: NonEmptyList[PatternCase[TContext, Wrap]]
  ) extends ArExpr[TContext, Wrap]

  final case class PrimitiveOp[TContext, Wrap[+_]]
  (
    operation: PrimitiveOperation,
    left: ArExprWrap[TContext, Wrap],
    right: ArExprWrap[TContext, Wrap],
    exprType: ArExprWrap[TContext, Wrap]
  ) extends ArExpr[TContext, Wrap]
  
  final case class Sequence[TContext, Wrap[+_]]
  (
    first: ArExprWrap[TContext, Wrap],
    second: ArExprWrap[TContext, Wrap]
  ) extends ArExpr[TContext, Wrap]
  
  final case class StoreVariable[TContext, Wrap[+_]]
  (
    variable: Variable[TContext, Wrap],
    value: ArExprWrap[TContext, Wrap],
    exprType: ArExprWrap[TContext, Wrap]
  ) extends ArExpr[TContext, Wrap]

  sealed trait TypeIsTypeOfTypeExpr[TContext, Wrap[+_]] extends ArExpr[TContext, Wrap]

  sealed trait TypeWithMethods[TContext, Wrap[+_]] extends TypeIsTypeOfTypeExpr[TContext, Wrap]

  final case class TypeOfType[TContext, Wrap[+_]]
  (
    inner: ArExprWrap[TContext, Wrap]
  ) extends TypeIsTypeOfTypeExpr[TContext, Wrap]
  
  final case class TypeN[TContext, Wrap[+_]]
  (
    universe: UniverseExpr, 
    subtypeConstraint: Option[Wrap[ArExpr[TContext, Wrap]]],
    supertypeConstraint: Option[Wrap[ArExpr[TContext, Wrap]]]
  ) extends TypeIsTypeOfTypeExpr[TContext, Wrap]

  final case class TraitType[TContext <: Context with Singleton, Wrap[+_]]
  (
    arTrait: AbsRef[TContext, ArTrait],
    args: Vector[ArExprWrap[TContext, Wrap]]
  ) extends TypeWithMethods[TContext, Wrap]
  
  final case class ClassType[TContext <: Context with Singleton, Wrap[+_]]
  (
    arClass: AbsRef[TContext, ArClass],
    args: Vector[ArExprWrap[TContext, Wrap]]
  ) extends TypeWithMethods[TContext, Wrap]
  
  final case class DataConstructorType[TContext <: Context with Singleton, Wrap[+_]]
  (
    ctor: AbsRef[TContext, DataConstructor],
    args: Vector[ArExprWrap[TContext, Wrap]],
    instanceType: TraitType[TContext, Wrap]
  ) extends TypeWithMethods[TContext, Wrap]

  final case class FunctionType[TContext, Wrap[+_]]
  (
    argumentType: ArExprWrap[TContext, Wrap],
    resultType: ArExprWrap[TContext, Wrap]
  ) extends TypeIsTypeOfTypeExpr[TContext, Wrap]
  
  final case class UnionType[TContext, Wrap[+_]]
  (
    first: ArExprWrap[TContext, Wrap],
    second: ArExprWrap[TContext, Wrap]
  ) extends TypeIsTypeOfTypeExpr[TContext, Wrap]
  
  final case class IntersectionType[TContext, Wrap[+_]]
  (
    first: ArExprWrap[TContext, Wrap],
    second: ArExprWrap[TContext, Wrap]
  ) extends TypeIsTypeOfTypeExpr[TContext, Wrap]

  final case class ExistentialType[TContext, Wrap[+_]]
  (
    variable: LocalVariable[TContext, Wrap],
    inner: ArExprWrap[TContext, Wrap]
  )

}
