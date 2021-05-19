package dev.argon.compiler.loaders.source

import dev.argon.compiler.Comp
import dev.argon.compiler.core.Context
import dev.argon.compiler.expr.ArExpr._
import dev.argon.compiler.expr.{FieldVariable, LocalVariable, ParameterVariable, PatternCase, ThisParameterVariable, Variable}
import dev.argon.compiler.types.TypeSystem
import shapeless.Nat
import zio.IO
import zio.interop.catz.core._
import cats.implicits._

sealed trait Erasure {

  val context: Context
  val typeSystem: TypeSystem.Aux[context.type]
  import typeSystem.WrapExpr

  private def isWrapExprErased(expr: WrapExpr): Comp[Boolean] =
    typeSystem.unwrapType(expr).existsM(isExprErased)

  private def isParamListErased[TResult[_ <: Context with Singleton, _[+_]]](sigComp: Comp[context.signatureContext.Signature[TResult, _ <: Nat]])(args: Vector[WrapExpr]): Comp[Boolean] =
    sigComp.flatMap { sig =>
      sig.unsubstitutedParameters.unsized.zip(args).existsM { case (param, arg) =>
        if(param.paramVar.isErased)
          IO.succeed(false)
        else
          isWrapExprErased(arg)
      }
    }

  def compOr(b1: Comp[Boolean], b2: => Comp[Boolean]): Comp[Boolean] =
    b1.flatMap {
      case true => IO.succeed(true)
      case false => b2
    }

  def compOr(b1: Comp[Boolean], b2: => Comp[Boolean], b3: => Comp[Boolean]): Comp[Boolean] =
    compOr(b1, compOr(b2, b3))

  def isVariableErased(variable: Variable[context.type, typeSystem.TTypeWrapper]): Boolean =
    variable match {
      case LocalVariable(_, _, _, _, isErased, _) => isErased
      case ParameterVariable(_, _, _, _, isErased, _) => isErased
      case ThisParameterVariable(_, _, _, _) => false
      case FieldVariable(_, _, _, _) => false
    }

  def isExprErased(expr: typeSystem.SimpleExpr): Comp[Boolean] =
    expr match {
      case ClassConstructorCall(_, classCtor, args) => isParamListErased(classCtor.value.signatureUnsubstituted)(args)
      case DataConstructorCall(dataCtorInstanceType, args) => isParamListErased(dataCtorInstanceType.ctor.value.signature)(args)
      case EnsureExecuted(body, ensuring) => compOr(isWrapExprErased(body), isWrapExprErased(ensuring))
      case FunctionCall(func, args, _) => isParamListErased(func.value.signature)(args)
      case FunctionObjectCall(function, arg, _) => compOr(isWrapExprErased(function), isWrapExprErased(arg))
      case IfElse(condition, ifBody, elseBody) => compOr(isWrapExprErased(condition), isWrapExprErased(ifBody), isWrapExprErased(elseBody))
      case LetBinding(variable, _, next) if variable.isErased => isWrapExprErased(next)
      case LetBinding(_, value, next) => compOr(isWrapExprErased(value), isWrapExprErased(next))
      case LoadConstantBool(_, _) => IO.succeed(false)
      case LoadConstantInt(_, _) => IO.succeed(false)
      case LoadConstantString(_, _) => IO.succeed(false)
      case LoadLambda(_, body) => isWrapExprErased(body)
      case LoadTuple(values) => values.existsM { case TupleElement(value) => isWrapExprErased(value) }
      case LoadTupleElement(tupleValue, _, _) => isWrapExprErased(tupleValue)
      case LoadUnit(_) => IO.succeed(false)
      case LoadVariable(variable) => IO.succeed(isVariableErased(variable))
      case MethodCall(method, instance, _, args, _) =>
        compOr(
          isWrapExprErased(instance),
          isParamListErased(method.value.signatureUnsubstituted)(args)
        )

      case PatternMatch(expr, cases) =>
        compOr(
          isWrapExprErased(expr),
          cases.existsM { case PatternCase(_, body) => isWrapExprErased(body) }
        )

      case Sequence(first, second) => compOr(isWrapExprErased(first), isWrapExprErased(second))
      case StoreVariable(variable, _, _) if isVariableErased(variable) => IO.succeed(false)
      case StoreVariable(_, value, _) => isWrapExprErased(value)
      case TypeOfType(_) => IO.succeed(true)
      case TypeN(_, _, _) => IO.succeed(true)
      case TraitType(arTrait, args) => isParamListErased(arTrait.value.signature)(args)
      case ClassType(arClass, args) => isParamListErased(arClass.value.signature)(args)
      case DataConstructorType(ctor, args, _) => isParamListErased(ctor.value.signature)(args)
      case FunctionType(_, _) => IO.succeed(true)
      case UnionType(_, _) => IO.succeed(true)
      case IntersectionType(_, _) => IO.succeed(true)
      case ExistentialType(_, _) => IO.succeed(true)
    }
}

object Erasure {
  type Aux[TContext <: Context with Singleton, TS <: TypeSystem.Aux[TContext]] = Erasure {
    val context: TContext
    val typeSystem: TS
  }

  def apply(ctx: Context)(ts: TypeSystem.Aux[ctx.type]): Erasure.Aux[ctx.type, ts.type] =
    new Erasure {
      override val context: ctx.type = ctx
      override val typeSystem: ts.type = ts
    }
}
