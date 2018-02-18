package com.mi3software.argon.compiler

import com.mi3software.argon.parser._
import com.mi3software.argon.util.{SourceLocation, WithSource}

import scalaz._
import Scalaz._

trait ExpressionConverter {

  type Conv[+T]
  protected implicit def monadInstance: Monad[Conv]

  protected def nextVariableId: Conv[Int]

  type TExprTypes <: ArExprTypes
  type TScopeTypes <: ScopeTypes
  type TS = TExprTypes#TS
  val typeComparer: TypeComparer[TS]

  protected def createTypeHole: Conv[TS#TType]
  protected def resolveType(t: TS#TType): Conv[TS#TType]

  type Env = ExpressionConvertEnvironment[TScopeTypes]

  protected sealed case class ArgumentInfo(argFactory: ExprFactory, location: SourceLocation)

  protected trait ExprFactory {
    def withExpectedType(expectedType: TS#TType): Conv[TExprTypes#TExpr]
    def accessMember(memberName: MemberName, location: SourceLocation): ExprFactory
    def forArguments(argInfo: ArgumentInfo): ExprFactory
  }

  protected def exprFactory(f: TS#TType => Conv[TExprTypes#TExpr]): ExprFactory

  protected def wrapExpr(expr: ArExpr[TExprTypes]): TExprTypes#TExpr

  def convertStatements(env: Env, stmts: WithSource[Vector[WithSource[Stmt]]]): ExprFactory = stmts.value match {
    case Vector() =>
      ???

    case WithSource(_: ClassDeclarationStmt | _: TraitDeclarationStmt | _: DataConstructorDeclarationStmt |
      _: FunctionDeclarationStmt | _: MethodDeclarationStmt | _: ClassConstructorDeclarationStmt |
      _: InitializeStmt | _: FieldDeclarationStmt | _: FieldInitializationStmt, _) +: _ =>
      ???

    case WithSource(VariableDeclarationStmt(isMutable, varType, name, value), location) +: tail =>
      exprFactory { expectedType =>
        for {
          varId <- nextVariableId
          varType <- varType.map(convertTypeExpression(env)).getOrElse(createTypeHole)
          valueExpr <- convertExpression(env, value).withExpectedType(varType)
          varTypeResolved <- resolveType(varType)
          tailExpr <- convertStatements(env, WithSource(tail, SourceLocation(location.end, stmts.location.end))).withExpectedType(expectedType)
        } yield wrapExpr(LetBinding(
          Variable(
            VariableDescriptor(env.owner, varId),
            name.map(VariableName.Normal).getOrElse(VariableName.Unnamed),
            Mutability.fromIsMutable(isMutable),
            varTypeResolved
          ),
          valueExpr,
          tailExpr
        ))
      }

    case Vector(WithSource(expr: Expr, location)) =>
      convertExpression(env, WithSource(expr, location))

    case WithSource(expr: Expr, location) +: tail =>
      exprFactory { expectedType =>
        for {
          ignoredType <- createTypeHole
          first <- convertExpression(env, WithSource(expr, location)).withExpectedType(ignoredType)
          second <- convertStatements(env, WithSource(tail, SourceLocation(location.end, stmts.location.end))).withExpectedType(expectedType)
        } yield wrapExpr(Sequence(first, second))
      }
  }

  def convertExpression(env: Env, expr: WithSource[Expr]): ExprFactory = expr.value match {
    case AsExpr(value, valueType) =>
      fromFixedTypeConv(env, expr.location)(
        convertTypeExpression(env)(valueType).flatMap(convertExpression(env, value).withExpectedType)
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
          argType <- convertTypeExpression(env)(argTypeExpr)
          resultType <- convertTypeExpression(env)(resultTypeExpr)
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

  def convertTypeExpression(env: Env)(expr: WithSource[Expr]): Conv[TS#TType]



  protected def fromFixedType(env: Env, location: SourceLocation)(expr: TExprTypes#TExpr): ExprFactory
  protected def fromFixedTypeConv(env: Env, location: SourceLocation)(expr: Conv[TExprTypes#TExpr]): ExprFactory

}

final case class ExpressionConvertEnvironment[Types <: ScopeTypes](owner: VariableOwnerDescriptor)


