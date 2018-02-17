package com.mi3software.argon.compiler

import com.mi3software.argon.parser._
import com.mi3software.argon.util.{SourceLocation, WithSource}

import scalaz._
import Scalaz._

trait ExpressionConverter {

  type Conv[+T]
  protected implicit def monadInstance: Monad[Conv]

  type TExprTypes <: ArExprTypes
  type TScopeTypes <: ScopeTypes
  type TS = TExprTypes#TS
  val typeComparer: TypeComparer[TS]

  type Env = ExpressionConvertEnvironment[TScopeTypes]

  protected sealed case class ArgumentInfo(argFactory: TypedExprFactory, location: SourceLocation)

  protected sealed trait TypedExprFactory {
    def withExpectedType(expectedType: TS#TType): Conv[TExprTypes#TExpr]
    def accessMember(memberName: MemberName, location: SourceLocation): TypedExprFactory
    def forArguments(argInfo: ArgumentInfo): TypedExprFactory
  }

  protected def wrapExpr(expr: ArExpr[TExprTypes]): TExprTypes#TExpr

  def convertExpression(env: Env, expr: WithSource[Expr]): TypedExprFactory = expr.value match {
    case AsExpr(value, valueType) =>
      fromFixedTypeConv(env, expr.location)(
        convertTypeExpression(env, valueType).flatMap(convertExpression(env, value).withExpectedType)
      )

    case BinaryOperatorExpr(_, _, _) => ???

    case BoolValueExpr(value) =>
      fromFixedType(env, expr.location)(wrapExpr(LoadConstantBool(value)))

    case ClassConstructorExpr(classExpr) => ???

    case DotExpr(baseExpr, memberName) =>
      convertExpression(env, baseExpr).accessMember(MemberName.Normal(memberName), expr.location)

    case FunctionCallExpr(func, arg) =>
      convertExpression(env, func).forArguments(ArgumentInfo(convertExpression(env, arg), expr.location))

    case IdentifierExpr(name) => ???

    case IfExpr(condition, body) => ???

    case IfElseExpr(condition, ifBody, elseBody) => ???

    case IntValueExpr(sign, base, digits) =>
      fromFixedTypeConv(env, expr.location)(
        for {
          intValue <- computeIntValue(sign, base, digits)
        } yield wrapExpr(LoadConstantInt(intValue))
      )

    case LambdaTypeExpr(argTypeExpr, resultTypeExpr) =>
      fromFixedTypeConv(env, expr.location)(
        for {
          argType <- convertTypeExpression(env, argTypeExpr)
          resultType <- convertTypeExpression(env, resultTypeExpr)
        } yield wrapExpr(LoadTypeValue[TExprTypes](typeComparer.typeBaseToType(FunctionType[TS](argType, resultType))))
      )

    case LambdaExpr(name, body) => ???

    case MatchExpr(value, cases) => ???

    case StringValueExpr(value) =>
      fromFixedType(env, expr.location)(wrapExpr(LoadConstantString(value)))

    case TupleExpr(values) =>
      ???

    case TypeExpr(instanceType, subtypeOf, supertypeOf) =>
      ???

    case TypeOfExpr(ofExpr) =>
      ???

    case UnaryOperatorExpr(_, _) => ???
  }

  private def computeIntValue(sign: Int, base: BigInt, digits: Vector[BigInt]): Conv[BigInt] = ???

  def convertTypeExpression(env: Env, expr: WithSource[Expr]): Conv[TS#TType]



  protected def fromFixedType(env: Env, location: SourceLocation)(expr: TExprTypes#TExpr): TypedExprFactory
  protected def fromFixedTypeConv(env: Env, location: SourceLocation)(expr: Conv[TExprTypes#TExpr]): TypedExprFactory

}

final case class ExpressionConvertEnvironment[Types <: ScopeTypes]()


