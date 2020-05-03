package dev.argon.compiler.core

import dev.argon.compiler.types.TypeSystem
import cats._
import cats.implicits._

trait Substitutions {

  type TContext <: Context with Singleton
  val typeSystem: TypeSystem[TContext]
  import typeSystem._

  val oldVariable: Variable
  val replacement: WrapExpr

  def substTypeArg(arg: TypeArgument): TypeArgument = arg match {
    case TypeArgument.Expr(argExpr) => TypeArgument.Expr(substWrapExpr(argExpr))
    case TypeArgument.Wildcard(u) => TypeArgument.Wildcard(u)
  }

  def substClassType(expr: ClassType): ClassType =
    ClassType(
      expr.arClass,
      expr.args.map(substTypeArg)
    )

  def substTraitType(expr: TraitType): TraitType =
    TraitType(
      expr.arTrait,
      expr.args.map(substTypeArg)
    )

  def substDataCtorType(expr: DataConstructorType): DataConstructorType =
    DataConstructorType(
      expr.ctor,
      expr.args.map(substTypeArg),
      substTraitType(expr.instanceType)
    )

  def substLocalVariable(localVariable: LocalVariable): LocalVariable =
    localVariable match {
      case LocalVariable(descriptor, name, mutability, varType) => LocalVariable(descriptor, name, mutability, substWrapExpr(varType))
    }

  def substParamVariable(paramVariable: ParameterVariable): ParameterVariable =
    paramVariable match {
      case ParameterVariable(descriptor, name, mutability, varType) => ParameterVariable(descriptor, name, mutability, substWrapExpr(varType))
    }

  def substVariable(variable: Variable): Variable =
    variable match {
      case variable: LocalVariable => substLocalVariable(variable)
      case variable: ParameterVariable => substParamVariable(variable)
      case FieldVariable(descriptor, ownerClass, name, mutability, varType) => FieldVariable(descriptor, ownerClass, name, mutability, substWrapExpr(varType))
    }

  def substParamElement(paramElem: ParameterElement): ParameterElement =
    ParameterElement(
      substParamVariable(paramElem.paramVar),
      paramElem.name,
      substWrapExpr(paramElem.elemType),
      paramElem.index,
    )

  def substParameter(parameter: Parameter): Parameter =
    Parameter(parameter.style, substParamVariable(parameter.paramVar), parameter.elements.map(substParamElement))

  def substArExpr(expr: ArExpr): WrapExpr =
    expr match {
      case ClassConstructorCall(classType, classCtor, args) =>
        fromSimpleType(ClassConstructorCall(substClassType(classType), classCtor, args.map(substWrapExpr)))

      case DataConstructorCall(dataCtorInstanceType, args) =>
        fromSimpleType(DataConstructorCall(substDataCtorType(dataCtorInstanceType), args.map(substWrapExpr)))

      case FunctionCall(function, args, returnType) =>
        fromSimpleType(FunctionCall(function, args.map(substWrapExpr), substWrapExpr(returnType)))

      case FunctionObjectCall(function, arg, returnType) =>
        fromSimpleType(FunctionObjectCall(substWrapExpr(function), substWrapExpr(arg), substWrapExpr(returnType)))

      case IfElse(condition, ifBody, elseBody) =>
        fromSimpleType(IfElse(substWrapExpr(condition), substWrapExpr(ifBody), substWrapExpr(elseBody)))

      case LetBinding(variable, value, next) =>
        fromSimpleType(LetBinding(substLocalVariable(variable), substWrapExpr(value), substWrapExpr(next)))

      case LoadConstantBool(value, exprType) => fromSimpleType(LoadConstantBool(value, substWrapExpr(exprType)))
      case LoadConstantInt(value, exprType) => fromSimpleType(LoadConstantInt(value, substWrapExpr(exprType)))
      case LoadConstantString(value, exprType) => fromSimpleType(LoadConstantString(value, substWrapExpr(exprType)))
      case LoadLambda(argVariable, body) => fromSimpleType(LoadLambda(substLocalVariable(argVariable), substWrapExpr(body)))
      case LoadTuple(values) => fromSimpleType(LoadTuple(values.map { case TupleElement(value) => TupleElement(substWrapExpr(value)) }))
      case LoadUnit(exprType) => fromSimpleType(LoadUnit(substWrapExpr(exprType)))
      case LoadVariable(variable) if variable.descriptor === oldVariable.descriptor => replacement
      case LoadVariable(variable) => fromSimpleType(LoadVariable(substVariable(variable)))
      case MethodCall(method, instance, args, returnType) =>
        fromSimpleType(MethodCall(method, substWrapExpr(instance), args.map(substWrapExpr), substWrapExpr(returnType)))

      case PrimitiveOp(operation, left, right, exprType) =>
        fromSimpleType(PrimitiveOp(operation, substWrapExpr(left), substWrapExpr(right), substWrapExpr(exprType)))

      case Sequence(first, second) =>
        fromSimpleType(Sequence(substWrapExpr(first), substWrapExpr(second)))

      case StoreVariable(variable, value, exprType) =>
        fromSimpleType(StoreVariable(substVariable(variable), substWrapExpr(value), substWrapExpr(exprType)))

      case expr: TraitType => fromSimpleType(substTraitType(expr))
      case expr: ClassType => fromSimpleType(substClassType(expr))
      case expr: DataConstructorType => fromSimpleType(substDataCtorType(expr))
      case TypeOfType(inner) => fromSimpleType(TypeOfType(substWrapExpr(inner)))
      case TypeN(universe, subtypeConstraint, supertypeConstraint) =>
        fromSimpleType(TypeN(
          universe,
          subtypeConstraint.map(substWrapExpr),
          supertypeConstraint.map(substWrapExpr)
        ))

      case FunctionType(argumentType, resultType) =>
        fromSimpleType(FunctionType(
          substWrapExpr(argumentType),
          substWrapExpr(resultType)
        ))

      case UnionType(first, second) =>
        fromSimpleType(UnionType(
          substWrapExpr(first),
          substWrapExpr(second)
        ))

      case IntersectionType(first, second) =>
        fromSimpleType(IntersectionType(
          substWrapExpr(first),
          substWrapExpr(second)
        ))
    }

  def substWrapExpr(expr: TType): TType =
    flatMapTypeWrapper(expr)(substArExpr)


}

object Substitutions {

  type Aux[TCtx <: Context with Singleton, TS <: TypeSystem[TCtx]] = Substitutions {
    type TContext = TCtx
    val typeSystem: TS
  }

  def apply[TCtx <: Context with Singleton](ts: TypeSystem[TCtx])(oldVar: ts.Variable, newExpr: ts.WrapExpr): Substitutions.Aux[TCtx, ts.type] =
    new Substitutions {
      override type TContext = TCtx
      override val typeSystem: ts.type = ts
      import typeSystem._

      override val oldVariable: Variable = oldVar
      override val replacement: WrapExpr = newExpr
    }

}