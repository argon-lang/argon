package dev.argon.compiler.core

import dev.argon.compiler.types.TypeSystem
import cats._
import cats.implicits._
import dev.argon.compiler.expr.ArExpr._
import dev.argon.compiler.expr._

sealed abstract class Substitutions[Wrap[+_]: Monad] {

  val context: Context

  val oldVariable: Variable[context.type, Wrap]
  val replacement: ArExprWrap[context.type, Wrap]

  private def fromSimpleType(expr: ArExpr[context.type, Wrap]): ArExprWrap[context.type, Wrap] =
    expr.pure[Wrap]

  def substClassType(expr: ClassType[context.type, Wrap]): ClassType[context.type, Wrap] =
    ClassType(
      expr.arClass,
      expr.args.map(substWrapExpr)
    )

  def substTraitType(expr: TraitType[context.type, Wrap]): TraitType[context.type, Wrap] =
    TraitType(
      expr.arTrait,
      expr.args.map(substWrapExpr)
    )

  def substDataCtorType(expr: DataConstructorType[context.type, Wrap]): DataConstructorType[context.type, Wrap] =
    DataConstructorType(
      expr.ctor,
      expr.args.map(substWrapExpr),
      substTraitType(expr.instanceType)
    )

  def substTypeWithMethods(expr: TypeWithMethods[context.type, Wrap]): TypeWithMethods[context.type, Wrap] =
    expr match {
      case expr @ TraitType(_, _) => substTraitType(expr)
      case expr @ ClassType(_, _) => substClassType(expr)
      case expr @ DataConstructorType(_, _, _) => substDataCtorType(expr)
    }

  def substLocalVariable(localVariable: LocalVariable[context.type, Wrap]): LocalVariable[context.type, Wrap] =
    localVariable match {
      case LocalVariable(id, owner, name, mutability, isErased, varType) => LocalVariable(id, owner, name, mutability, isErased, substWrapExpr(varType))
    }

  def substParamVariable(paramVariable: ParameterVariable[context.type, Wrap]): ParameterVariable[context.type, Wrap] =
    paramVariable match {
      case ParameterVariable(id, owner, name, mutability, isErased, varType) => ParameterVariable(id, owner, name, mutability, isErased, substWrapExpr(varType))
    }

  def substVariable(variable: Variable[context.type, Wrap]): Variable[context.type, Wrap] =
    variable match {
      case variable: LocalVariable[context.type, Wrap] => substLocalVariable(variable)
      case variable: ParameterVariable[context.type, Wrap] => substParamVariable(variable)
      case ThisParameterVariable(owner, name, mutability, varType) => ThisParameterVariable(owner, name, mutability, substWrapExpr(varType))
      case FieldVariable(owner, name, mutability, varType) => FieldVariable(owner, name, mutability, substWrapExpr(varType))
    }

  def substParamElement(paramElem: ParameterElement[context.type, Wrap]): ParameterElement[context.type, Wrap] =
    ParameterElement(
      substParamVariable(paramElem.paramVar),
      paramElem.name,
      substWrapExpr(paramElem.elemType),
      paramElem.index,
    )

  def substParameter(parameter: Parameter[context.type, Wrap]): Parameter[context.type, Wrap] =
    Parameter(parameter.style, substParamVariable(parameter.paramVar), parameter.elements.map(substParamElement))

  def substArExpr(expr: ArExpr[context.type, Wrap]): ArExprWrap[context.type, Wrap] =
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
      case LoadVariable(variable) if variable === oldVariable => replacement
      case LoadVariable(variable) => fromSimpleType(LoadVariable(substVariable(variable)))
      case MethodCall(method, instance, instanceType, args, returnType) =>
        fromSimpleType(MethodCall(method, substWrapExpr(instance), substTypeWithMethods(instanceType), args.map(substWrapExpr), substWrapExpr(returnType)))

      case Sequence(first, second) =>
        fromSimpleType(Sequence(substWrapExpr(first), substWrapExpr(second)))

      case StoreVariable(variable, value, exprType) =>
        fromSimpleType(StoreVariable(substVariable(variable), substWrapExpr(value), substWrapExpr(exprType)))

      case expr: TypeWithMethods[context.type, Wrap] =>
        fromSimpleType(substTypeWithMethods(expr))

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

  def substWrapExpr(expr: ArExprWrap[context.type, Wrap]): ArExprWrap[context.type, Wrap] =
    expr.flatMap(substArExpr)


}

object Substitutions {

  type Aux[TCtx <: Context with Singleton, Wrap[+_]] = Substitutions[Wrap] {
    val context: TCtx
  }

  def apply[Wrap[+_]: Monad](ctx: Context)(oldVar: Variable[ctx.type, Wrap], newExpr: ArExprWrap[ctx.type, Wrap]): Substitutions.Aux[ctx.type, Wrap] =
    new Substitutions[Wrap] {
      override val context: ctx.type = ctx

      override val oldVariable: Variable[context.type, Wrap] = oldVar
      override val replacement: ArExprWrap[context.type, Wrap] = newExpr
    }

}