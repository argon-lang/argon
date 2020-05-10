package dev.argon.compiler.types

import cats._
import cats.implicits._
import dev.argon.compiler.Comp
import dev.argon.compiler.core._
import dev.argon.compiler.expr._
import dev.argon.compiler.expr.ArExpr._
import dev.argon.util.AnyExtensions._
import zio.IO
import zio.interop.catz._

abstract class TypeSystemConverter {
  val context: Context
  type FromWrap[+_]
  type ToWrap[+_]

  protected implicit val fromWrapInstances: WrapperInstance[FromWrap]
  protected implicit val toWrapInstances: WrapperInstance[ToWrap]

  protected def convertType[A](fromExpr: ArExpr[context.type, FromWrap] => Comp[A])(t: FromWrap[A]): Comp[ToWrap[A]]

  def convertTypeSystem
  (t1: ArExprWrap[context.type, FromWrap])
  : Comp[ArExprWrap[context.type, ToWrap]] =
    t1.traverse(convertExprTypeSystem(_))
      .flatMap(convertType(convertExprTypeSystem)(_))

  def convertTypeArg
  (t1: TypeArgument[context.type, FromWrap])
  : Comp[TypeArgument[context.type, ToWrap]] =
    t1 match {
      case TypeArgument.Expr(expr) =>
        convertTypeSystem(expr).map(TypeArgument.Expr.apply)

      case TypeArgument.Wildcard(u) =>
        IO.succeed(TypeArgument.Wildcard(u))
    }

  def convertTraitType
  (traitType: TraitType[context.type, FromWrap])
  : Comp[TraitType[context.type, ToWrap]] = for {
    newArgs <- traitType.args.traverse(convertTypeArg(_))
  } yield TraitType(
    traitType.arTrait,
    newArgs,
  )

  def convertClassType
  (classType: ClassType[context.type, FromWrap])
  : Comp[ClassType[context.type, ToWrap]] = for {
    newArgs <- classType.args.traverse(convertTypeArg(_))
  } yield ClassType(
    classType.arClass,
    newArgs
  )


  def convertDataConstructorType
  (dataCtorType: DataConstructorType[context.type, FromWrap])
  : Comp[DataConstructorType[context.type, ToWrap]] = for {
    newArgs <- dataCtorType.args.traverse(convertTypeArg(_))
    newInstanceType <- convertTraitType(dataCtorType.instanceType)
  } yield DataConstructorType(
    dataCtorType.ctor,
    newArgs,
    newInstanceType
  )

  final def convertLocalVariableTypeSystem
  (v: LocalVariable[context.type, FromWrap])
  : Comp[LocalVariable[context.type, ToWrap]] =
    for {
      newType <- convertTypeSystem(v.varType)
    } yield LocalVariable(v.descriptor, v.name, v.mutability, newType)

  final def convertParamVariableTypeSystem
  (v: ParameterVariable[context.type, FromWrap])
  : Comp[ParameterVariable[context.type, ToWrap]] =
    for {
      newType <- convertTypeSystem(v.varType)
    } yield ParameterVariable(v.descriptor, v.name, v.mutability, newType)

  final def convertVariableTypeSystem
  (v: Variable[context.type, FromWrap])
  : Comp[Variable[context.type, ToWrap]] =
    v match {
      case v @ LocalVariable(_, _, _, _) =>
        convertLocalVariableTypeSystem(v).map(identity)

      case v @ ParameterVariable(_, _, _, _) =>
        convertParamVariableTypeSystem(v).map(identity)

      case FieldVariable(descriptor, ownerClass, name, mutability, varType) =>
        for {
          newType <- convertTypeSystem(varType)
        } yield FieldVariable(descriptor, ownerClass, name, mutability, newType)

    }

  final def convertParameterElementTypeSystem
  (p: ParameterElement[context.type, FromWrap])
  : Comp[ParameterElement[context.type, ToWrap]] =
    for {
      newParamVar <- convertParamVariableTypeSystem(p.paramVar)
      newElemType <- convertTypeSystem(p.elemType)
    } yield ParameterElement(newParamVar, p.name, newElemType, p.index)

  final def convertParameterTypeSystem
  (p: Parameter[context.type, FromWrap])
  : Comp[Parameter[context.type, ToWrap]] =
    for {
      newParamVar <- convertParamVariableTypeSystem(p.paramVar)
      newElems <- p.elements.traverse(convertParameterElementTypeSystem(_))
    } yield Parameter(p.style, newParamVar, newElems)

  def convertPatternExprTypeSystem
  (pattern: PatternExpr[context.type, FromWrap])
  : Comp[PatternExpr[context.type, ToWrap]] =
    pattern match {
      case PatternExpr.DataDeconstructor(ctor, args) =>
        for {
          newArgs <- args.traverse(convertPatternExprTypeSystem(_))
        } yield PatternExpr.DataDeconstructor(ctor, newArgs)

      case PatternExpr.Binding(variable) =>
        for {
          newVar <- convertLocalVariableTypeSystem(variable)
        } yield PatternExpr.Binding(newVar)

      case PatternExpr.CastBinding(variable) =>
        for {
          newVar <- convertLocalVariableTypeSystem(variable)
        } yield PatternExpr.CastBinding(newVar)
    }

  def convertExprTypeSystem
  (expr: ArExpr[context.type, FromWrap])
  : Comp[ArExpr[context.type, ToWrap]] = expr match {


    case ClassConstructorCall(classType, classCtor, args) =>
      for {
        newClassType <- convertClassType(classType)
        newArgs <- args.traverse(convertTypeSystem(_))
      } yield ClassConstructorCall(newClassType, classCtor, newArgs)

    case DataConstructorCall(dataCtor, args) =>
      for {
        newType <- convertDataConstructorType(dataCtor)
        newArgs <- args.traverse(convertTypeSystem(_))
      } yield DataConstructorCall(newType, newArgs)

    case EnsureExecuted(body, ensuring) =>
      for {
        newBody <- convertTypeSystem(body)
        newEnsuring <- convertTypeSystem(ensuring)
      } yield EnsureExecuted(newBody, newEnsuring)

    case FunctionCall(function, args, returnType) =>
      for {
        newArgs <- args.traverse(convertTypeSystem(_))
        newReturnType <- convertTypeSystem(returnType)
      } yield FunctionCall(function, newArgs, newReturnType)

    case FunctionObjectCall(function, args, returnType) =>
      for {
        newFunction <- convertTypeSystem(function)
        newArgs <- convertTypeSystem(args)
        newReturnType <- convertTypeSystem(returnType)
      } yield FunctionObjectCall(newFunction, newArgs, newReturnType)

    case IfElse(condition, ifBody, elseBody) =>
      for {
        newCondition <- convertTypeSystem(condition)
        newIfBody <- convertTypeSystem(ifBody)
        newElseBody <- convertTypeSystem(elseBody)
      } yield IfElse(newCondition, newIfBody, newElseBody)

    case LetBinding(variable, value, next) =>
      for {
        newVar <- convertLocalVariableTypeSystem(variable)
        newValue <- convertTypeSystem(value)
        newNext <- convertTypeSystem(next)
      } yield LetBinding(newVar, newValue, newNext)

    case LoadConstantBool(value, exprType) =>
      for {
        newType <- convertTypeSystem(exprType)
      } yield LoadConstantBool(value, newType)

    case LoadConstantInt(value, exprType) =>
      for {
        newType <- convertTypeSystem(exprType)
      } yield LoadConstantInt(value, newType)

    case LoadConstantString(value, exprType) =>
      for {
        newType <- convertTypeSystem(exprType)
      } yield LoadConstantString(value, newType)

    case LoadLambda(variable, body) =>
      for {
        newVar <- convertLocalVariableTypeSystem(variable)
        newBody <- convertTypeSystem(body)
      } yield LoadLambda(newVar, newBody)

    case LoadTuple(tupleValues) =>
      tupleValues
        .traverse { case TupleElement(elementType) =>
          convertTypeSystem(elementType).map(TupleElement(_))
        }
        .map(LoadTuple(_))

    case LoadTupleElement(tupleValue, elemType, index) =>
      for {
        newValue <- convertTypeSystem(tupleValue)
        newType <- convertTypeSystem(elemType)
      } yield LoadTupleElement(newValue, newType, index)

    case LoadUnit(exprType) =>
      for {
        newType <- convertTypeSystem(exprType)
      } yield LoadUnit(newType)

    case LoadVariable(variable) =>
      for {
        newVar <- convertVariableTypeSystem(variable)
      } yield LoadVariable(newVar)

    case MethodCall(method, instance, args, returnType) =>
      for {
        newInstance <- convertTypeSystem(instance)
        newArgs <- args.traverse(convertTypeSystem(_))
        newReturnType <- convertTypeSystem(returnType)
      } yield MethodCall(method, newInstance, newArgs, newReturnType)

    case PatternMatch(expr, cases) =>
      for {
        newExpr <- convertTypeSystem(expr)
        newCases <- cases.traverse {
          case PatternCase(pattern, body) =>
            for {
              newPattern <- convertPatternExprTypeSystem(pattern)
              newBody <- convertTypeSystem(body)
            } yield PatternCase(newPattern, newBody)
        }
      } yield PatternMatch(newExpr, newCases)

    case PrimitiveOp(operation, left, right, exprType) =>
      for {
        newLeft <- convertTypeSystem(left)
        newRight <- convertTypeSystem(right)
        newType <- convertTypeSystem(exprType)
      } yield PrimitiveOp(operation, newLeft, newRight, newType)

    case Sequence(first, second) =>
      for {
        newFirst <- convertTypeSystem(first)
        newSecond <- convertTypeSystem(second)
      } yield Sequence(newFirst, newSecond)

    case StoreVariable(variable, value, unitType) =>
      for {
        newVar <- convertVariableTypeSystem(variable)
        newValue <- convertTypeSystem(value)
        newUnitType <- convertTypeSystem(unitType)
      } yield StoreVariable(newVar, newValue, newUnitType)

    case t1 @ TraitType(_, _) => convertTraitType(t1)
    case t1 @ ClassType(_, _) => convertClassType(t1)
    case t1 @ DataConstructorType(_, _, _) => convertDataConstructorType(t1)

    case TypeOfType(inner) =>
      for {
        newInner <- convertTypeSystem(inner)
      } yield TypeOfType(newInner)

    case TypeN(universe, subtypeConstraint, supertypeConstraint) =>
      for {
        newSub <- subtypeConstraint.traverse(convertTypeSystem(_))
        newSup <- supertypeConstraint.traverse(convertTypeSystem(_))
      } yield TypeN(universe, newSub, newSup)

    case LoadVariable(variable) =>
      for {
        newVar <- convertVariableTypeSystem(variable)
      } yield LoadVariable(newVar)

    case FunctionType(argumentType, resultType) =>
      for {
        newArgType <- convertTypeSystem(argumentType)
        newResultType <- convertTypeSystem(resultType)
      } yield FunctionType(newArgType, newResultType)

    case UnionType(first, second) =>
      for {
        newFirst <- convertTypeSystem(first)
        newSecond <- convertTypeSystem(second)
      } yield UnionType(newFirst, newSecond)

    case IntersectionType(first, second) =>
      for {
        newFirst <- convertTypeSystem(first)
        newSecond <- convertTypeSystem(second)
      } yield IntersectionType(newFirst, newSecond)
  }



}

object TypeSystemConverter {

  type Aux[TContext <: Context with Singleton, TFromWrap[+_], TToWrap[+_]] =
    TypeSystemConverter {
      val context: TContext
      type FromWrap[+A] = TFromWrap[A]
      type ToWrap[+A] = TToWrap[A]
    }

}
