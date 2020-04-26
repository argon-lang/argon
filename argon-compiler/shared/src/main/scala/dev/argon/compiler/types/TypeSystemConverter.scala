package dev.argon.compiler.types

import cats._
import cats.implicits._
import dev.argon.compiler.Comp
import dev.argon.compiler.core._
import dev.argon.util.AnyExtensions._
import zio.interop.catz._

abstract class TypeSystemConverter {
  val context: Context
  val ts: TypeSystem[context.type]
  val otherTS: TypeSystem[context.type]

  protected def convertType[A](fromExpr: otherTS.ArExpr => A)(t: ts.TTypeWrapper[A]): Comp[otherTS.TTypeWrapper[A]]

  def convertTypeSystem
  (t1: ts.TType)
  : Comp[otherTS.TType] =
    ts.traverseTypeWrapper(t1)(convertExprTypeSystem(_))
      .flatMap(convertType(identity)(_))

  def convertTypeArg
  (t1: ts.TypeArgument)
  : Comp[otherTS.TypeArgument] =
    t1 match {
      case ts.TypeArgument.Expr(expr) =>
        convertTypeSystem(expr).map(otherTS.TypeArgument.Expr)

      case ts.TypeArgument.Wildcard(u) =>
        convertUniverseTypeSystem(u).map(otherTS.TypeArgument.Wildcard)
    }

  def convertTraitType
  (traitType: ts.TraitType)
  : Comp[otherTS.TraitType] = for {
    newArgs <- traitType.args.traverse(convertTypeArg(_))
  } yield otherTS.TraitType(
    traitType.arTrait,
    newArgs,
  )

  def convertClassType
  (classType: ts.ClassType)
  : Comp[otherTS.ClassType] = for {
    newArgs <- classType.args.traverse(convertTypeArg(_))
  } yield otherTS.ClassType(
    classType.arClass,
    newArgs
  )


  def convertDataConstructorType
  (dataCtorType: ts.DataConstructorType)
  : Comp[otherTS.DataConstructorType] = for {
    newArgs <- dataCtorType.args.traverse(convertTypeArg(_))
    newInstanceType <- convertTraitType(dataCtorType.instanceType)
  } yield otherTS.DataConstructorType(
    dataCtorType.ctor,
    newArgs,
    newInstanceType
  )

  final def convertLocalVariableTypeSystem
  (v: ts.LocalVariable)
  : Comp[otherTS.LocalVariable] =
    for {
      newType <- convertTypeSystem(v.varType)
    } yield otherTS.LocalVariable(v.descriptor, v.name, v.mutability, newType)

  final def convertParamVariableTypeSystem
  (v: ts.ParameterVariable)
  : Comp[otherTS.ParameterVariable] =
    for {
      newType <- convertTypeSystem(v.varType)
    } yield otherTS.ParameterVariable(v.descriptor, v.name, v.mutability, newType)

  final def convertVariableTypeSystem
  (v: ts.Variable)
  : Comp[otherTS.Variable] =
    v match {
      case v @ ts.LocalVariable(_, _, _, _) =>
        convertLocalVariableTypeSystem(v).map(identity)

      case v @ ts.ParameterVariable(_, _, _, _) =>
        convertParamVariableTypeSystem(v).map(identity)

      case ts.FieldVariable(descriptor, ownerClass, name, mutability, varType) =>
        for {
          newType <- convertTypeSystem(varType)
        } yield otherTS.FieldVariable(descriptor, ownerClass, name, mutability, newType)

    }

  final def convertParameterElementTypeSystem
  (p: ts.ParameterElement)
  : Comp[otherTS.ParameterElement] =
    for {
      newParamVar <- convertParamVariableTypeSystem(p.paramVar)
      newElemType <- convertTypeSystem(p.elemType)
    } yield otherTS.ParameterElement(newParamVar, p.name, newElemType, p.index)

  final def convertParameterTypeSystem
  (p: ts.Parameter)
  : Comp[otherTS.Parameter] =
    for {
      newParamVar <- convertParamVariableTypeSystem(p.paramVar)
      newElems <- p.elements.traverse(convertParameterElementTypeSystem(_))
    } yield otherTS.Parameter(p.style, newParamVar, newElems)

  def convertPatternExprTypeSystem
  (pattern: ts.PatternExpr)
  : Comp[otherTS.PatternExpr] =
    pattern match {
      case ts.PatternExpr.DataDeconstructor(ctor, args) =>
        for {
          newArgs <- args.traverse(convertPatternExprTypeSystem(_))
        } yield otherTS.PatternExpr.DataDeconstructor(ctor, newArgs)

      case ts.PatternExpr.Binding(variable) =>
        for {
          newVar <- convertLocalVariableTypeSystem(variable)
        } yield otherTS.PatternExpr.Binding(newVar)

      case ts.PatternExpr.CastBinding(variable) =>
        for {
          newVar <- convertLocalVariableTypeSystem(variable)
        } yield otherTS.PatternExpr.CastBinding(newVar)
    }

  def convertUniverseTypeSystem
  (expr: ts.UniverseExpr)
  : Comp[otherTS.UniverseExpr] = expr match {
    case ts.FixedUniverse(u) => otherTS.FixedUniverse(u).upcast[otherTS.UniverseExpr].pure[Comp]
    case ts.AbstractUniverse() => otherTS.AbstractUniverse().upcast[otherTS.UniverseExpr].pure[Comp]

    case ts.LargestUniverse(a, b) =>
      for {
        newA <- convertUniverseTypeSystem(a)
        newB <- convertUniverseTypeSystem(b)
      } yield otherTS.LargestUniverse(newA, newB)

    case ts.NextLargestUniverse(a) =>
      for {
        newA <- convertUniverseTypeSystem(a)
      } yield otherTS.NextLargestUniverse(newA)

    case ts.PreviousUniverse(a) =>
      for {
        newA <- convertUniverseTypeSystem(a)
      } yield otherTS.PreviousUniverse(newA)
  }

  def convertExprTypeSystem
  (expr: ts.ArExpr)
  : Comp[otherTS.ArExpr] = expr match {


    case ts.ClassConstructorCall(classType, classCtor, args) =>
      for {
        newClassType <- convertClassType(classType)
        newArgs <- args.traverse(convertTypeSystem(_))
      } yield otherTS.ClassConstructorCall(newClassType, classCtor, newArgs)

    case ts.DataConstructorCall(dataCtor, args) =>
      for {
        newType <- convertDataConstructorType(dataCtor)
        newArgs <- args.traverse(convertTypeSystem(_))
      } yield otherTS.DataConstructorCall(newType, newArgs)

    case ts.EnsureExecuted(body, ensuring) =>
      for {
        newBody <- convertTypeSystem(body)
        newEnsuring <- convertTypeSystem(ensuring)
      } yield otherTS.EnsureExecuted(newBody, newEnsuring)

    case ts.FunctionCall(function, args, returnType) =>
      for {
        newArgs <- args.traverse(convertTypeSystem(_))
        newReturnType <- convertTypeSystem(returnType)
      } yield otherTS.FunctionCall(function, newArgs, newReturnType)

    case ts.FunctionObjectCall(function, args, returnType) =>
      for {
        newFunction <- convertTypeSystem(function)
        newArgs <- convertTypeSystem(args)
        newReturnType <- convertTypeSystem(returnType)
      } yield otherTS.FunctionObjectCall(newFunction, newArgs, newReturnType)

    case ts.IfElse(condition, ifBody, elseBody) =>
      for {
        newCondition <- convertTypeSystem(condition)
        newIfBody <- convertTypeSystem(ifBody)
        newElseBody <- convertTypeSystem(elseBody)
      } yield otherTS.IfElse(newCondition, newIfBody, newElseBody)

    case ts.LetBinding(variable, value, next) =>
      for {
        newVar <- convertLocalVariableTypeSystem(variable)
        newValue <- convertTypeSystem(value)
        newNext <- convertTypeSystem(next)
      } yield otherTS.LetBinding(newVar, newValue, newNext)

    case ts.LoadConstantBool(value, exprType) =>
      for {
        newType <- convertTypeSystem(exprType)
      } yield otherTS.LoadConstantBool(value, newType)

    case ts.LoadConstantInt(value, exprType) =>
      for {
        newType <- convertTypeSystem(exprType)
      } yield otherTS.LoadConstantInt(value, newType)

    case ts.LoadConstantString(value, exprType) =>
      for {
        newType <- convertTypeSystem(exprType)
      } yield otherTS.LoadConstantString(value, newType)

    case ts.LoadLambda(variable, body) =>
      for {
        newVar <- convertLocalVariableTypeSystem(variable)
        newBody <- convertTypeSystem(body)
      } yield otherTS.LoadLambda(newVar, newBody)

    case expr: ts.LoadTuple =>
      expr.values
        .traverse { case ts.TupleElement(elementType) =>
          convertTypeSystem(elementType).map(otherTS.TupleElement(_))
        }
        .map(otherTS.LoadTuple(_))

    case ts.LoadTupleElement(tupleValue, elemType, index) =>
      for {
        newValue <- convertTypeSystem(tupleValue)
        newType <- convertTypeSystem(elemType)
      } yield otherTS.LoadTupleElement(newValue, newType, index)

    case ts.LoadUnit(exprType) =>
      for {
        newType <- convertTypeSystem(exprType)
      } yield otherTS.LoadUnit(newType)

    case ts.LoadVariable(variable) =>
      for {
        newVar <- convertVariableTypeSystem(variable)
      } yield otherTS.LoadVariable(newVar)

    case ts.MethodCall(method, instance, args, returnType) =>
      for {
        newInstance <- convertTypeSystem(instance)
        newArgs <- args.traverse(convertTypeSystem(_))
        newReturnType <- convertTypeSystem(returnType)
      } yield otherTS.MethodCall(method, newInstance, newArgs, newReturnType)

    case ts.PatternMatch(expr, cases) =>
      for {
        newExpr <- convertTypeSystem(expr)
        newCases <- cases.traverse {
          case ts.PatternCase(pattern, body) =>
            for {
              newPattern <- convertPatternExprTypeSystem(pattern)
              newBody <- convertTypeSystem(body)
            } yield otherTS.PatternCase(newPattern, newBody)
        }
      } yield otherTS.PatternMatch(newExpr, newCases)

    case ts.PrimitiveOp(operation, left, right, exprType) =>
      for {
        newLeft <- convertTypeSystem(left)
        newRight <- convertTypeSystem(right)
        newType <- convertTypeSystem(exprType)
      } yield otherTS.PrimitiveOp(operation, newLeft, newRight, newType)

    case ts.Sequence(first, second) =>
      for {
        newFirst <- convertTypeSystem(first)
        newSecond <- convertTypeSystem(second)
      } yield otherTS.Sequence(newFirst, newSecond)

    case ts.StoreVariable(variable, value, unitType) =>
      for {
        newVar <- convertVariableTypeSystem(variable)
        newValue <- convertTypeSystem(value)
        newUnitType <- convertTypeSystem(unitType)
      } yield otherTS.StoreVariable(newVar, newValue, newUnitType)

    case t1: ts.TraitType => convertTraitType(t1).map(identity)
    case t1: ts.ClassType => convertClassType(t1).map(identity)
    case t1: ts.DataConstructorType => convertDataConstructorType(t1).map(identity)

    case ts.TypeOfType(inner) =>
      for {
        newInner <- convertTypeSystem(inner)
      } yield (otherTS.TypeOfType(newInner) : otherTS.ArExpr)

    case ts.TypeN(universe, subtypeConstraint, supertypeConstraint) =>
      for {
        newUniv <- convertUniverseTypeSystem(universe)
        newSub <- subtypeConstraint.traverse(convertTypeSystem(_))
        newSup <- supertypeConstraint.traverse(convertTypeSystem(_))
      } yield (otherTS.TypeN(newUniv, newSub, newSup) : otherTS.ArExpr)

    case ts.LoadVariable(variable) =>
      for {
        newVar <- convertVariableTypeSystem(variable)
      } yield otherTS.LoadVariable(newVar)

    case ts.FunctionType(argumentType, resultType) =>
      for {
        newArgType <- convertTypeSystem(argumentType)
        newResultType <- convertTypeSystem(resultType)
      } yield otherTS.FunctionType(newArgType, newResultType)

    case ts.UnionType(first, second) =>
      for {
        newFirst <- convertTypeSystem(first)
        newSecond <- convertTypeSystem(second)
      } yield otherTS.UnionType(newFirst, newSecond)

    case ts.IntersectionType(first, second) =>
      for {
        newFirst <- convertTypeSystem(first)
        newSecond <- convertTypeSystem(second)
      } yield otherTS.IntersectionType(newFirst, newSecond)
  }



}

object TypeSystemConverter {

  type Aux[TContext <: Context with Singleton, TS1 <: TypeSystem[TContext], TS2 <: TypeSystem[TContext]] =
    TypeSystemConverter {
      val context: TContext
      val ts: TS1
      val otherTS: TS2
    }

}
