package dev.argon.compiler.core

import dev.argon.compiler.types.TypeSystem
import cats._
import cats.implicits._

trait Substitutions {

  type TContext <: Context with Singleton
  val typeSystem: TypeSystem[TContext]
  import typeSystem._

  val oldVariable: Variable
  val replacement: ArExpr

  def substTypeArg(arg: TypeArgument): TypeArgument = arg match {
    case TypeArgument.Expr(argExpr) => TypeArgument.Expr(substWrapExpr(argExpr))
    case TypeArgument.Wildcard(u) => TypeArgument.Wildcard(u)
  }

  def substClassType(expr: ClassType): ClassType =
    ClassType(
      expr.arClass,
      expr.args.map(substTypeArg),
      BaseTypeInfoClass(
        expr.baseTypes.baseClass.map(substClassType),
        expr.baseTypes.baseTraits.map(substTraitType)
      )
    )

  def substTraitType(expr: TraitType): TraitType =
    TraitType(
      expr.arTrait,
      expr.args.map(substTypeArg),
      BaseTypeInfoTrait(
        expr.baseTypes.baseTraits.map(substTraitType)
      )
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
    Parameter(substParamVariable(parameter.paramVar), parameter.elements.map(substParamElement))

  def substArExpr(expr: ArExpr): ArExpr =
    expr match {
      case ClassConstructorCall(classType, classCtor, args) =>
        ClassConstructorCall(substClassType(classType), classCtor, args.map(substArExpr))

      case DataConstructorCall(dataCtorInstanceType, args) =>
        DataConstructorCall(substDataCtorType(dataCtorInstanceType), args.map(substArExpr))

      case FunctionCall(function, args, returnType) =>
        FunctionCall(function, args.map(substArExpr), substWrapExpr(returnType))

      case FunctionObjectCall(function, arg, returnType) =>
        FunctionObjectCall(substArExpr(function), substArExpr(arg), substWrapExpr(returnType))

      case IfElse(condition, ifBody, elseBody) =>
        IfElse(substArExpr(condition), substArExpr(ifBody), substArExpr(elseBody))

      case LetBinding(variable, value, next) =>
        LetBinding(substLocalVariable(variable), substArExpr(value), substArExpr(next))

      case LoadConstantBool(value, exprType) => LoadConstantBool(value, substWrapExpr(exprType))
      case LoadConstantInt(value, exprType) => LoadConstantInt(value, substWrapExpr(exprType))
      case LoadConstantString(value, exprType) => LoadConstantString(value, substWrapExpr(exprType))
      case LoadLambda(argVariable, body) => LoadLambda(substLocalVariable(argVariable), substArExpr(body))
      case LoadTuple(values) => LoadTuple(values.map { case TupleElement(value) => TupleElement(substWrapExpr(value)) })
      case LoadUnit(exprType) => LoadUnit(substWrapExpr(exprType))
      case LoadVariable(variable) if variable.descriptor === oldVariable.descriptor => replacement
      case LoadVariable(variable) => LoadVariable(substVariable(variable))
      case MethodCall(method, instance, args, returnType) =>
        MethodCall(method, substArExpr(instance), args.map(substArExpr), substWrapExpr(returnType))

      case PrimitiveOp(operation, left, right, exprType) =>
        PrimitiveOp(operation, substArExpr(left), substArExpr(right), substWrapExpr(exprType))

      case Sequence(first, second) =>
        Sequence(substArExpr(first), substArExpr(second))

      case StoreVariable(variable, value, exprType) =>
        StoreVariable(substVariable(variable), substArExpr(value), substWrapExpr(exprType))

      case expr: TraitType => substTraitType(expr)
      case expr: ClassType => substClassType(expr)
      case expr: DataConstructorType => substDataCtorType(expr)
      case TypeOfType(inner) => TypeOfType(substWrapExpr(inner))
      case TypeN(universe, subtypeConstraint, supertypeConstraint) =>
        TypeN(
          universe,
          subtypeConstraint.map(substWrapExpr),
          supertypeConstraint.map(substWrapExpr)
        )

      case FunctionType(argumentType, resultType) =>
        FunctionType(
          substWrapExpr(argumentType),
          substWrapExpr(resultType)
        )

      case UnionType(first, second) =>
        UnionType(
          substWrapExpr(first),
          substWrapExpr(second)
        )

      case IntersectionType(first, second) =>
        IntersectionType(
          substWrapExpr(first),
          substWrapExpr(second)
        )
    }

  def substWrapExpr(expr: TType): TType =
    mapTypeWrapper(expr)(substArExpr)


}

object Substitutions {

  type Aux[TCtx <: Context with Singleton, TS <: TypeSystem[TCtx]] = Substitutions {
    type TContext = TCtx
    val typeSystem: TS
  }

  def apply[TCtx <: Context with Singleton](ts: TypeSystem[TCtx])(oldVar: ts.Variable, newExpr: ts.ArExpr): Substitutions.Aux[TCtx, ts.type] =
    new Substitutions {
      override type TContext = TCtx
      override val typeSystem: ts.type = ts
      import typeSystem._

      override val oldVariable: Variable = oldVar
      override val replacement: ArExpr = newExpr
    }

}