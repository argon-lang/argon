package com.mi3software.argon.compiler

import com.mi3software.argon.parser._
import com.mi3software.argon.util.{Compilation, FileSpec, SourceLocation, WithSource}

import scalaz._
import Scalaz._
import ScopeHelpers._

trait ExpressionConverter {

  type Conv[+T]
  protected implicit def monadInstance: Monad[Conv]
  protected implicit def compilationInstance: Compilation[Conv]

  protected def nextVariableId: Conv[Int]

  type TExprTypes <: ArExprTypes
  type TS = TExprTypes#TS

  type TScopeTypes <: ScopeTypes with ({
    type TFunc = TExprTypes#TFunction
    type TVariable = Variable[TS, VariableLikeDescriptor]
  })

  val typeComparer: TypeComparer[TS]

  val scopeLookupComparer: LookupComparer[ScopeValue[TScopeTypes]]

  protected def createTypeHole: Conv[TS#TType]
  protected def resolveType(t: TS#TType): Conv[TS#TType]

  type Env = ExpressionConvertEnvironment[TScopeTypes]

  protected sealed case class ArgumentInfo(argFactory: ExprFactory, location: SourceLocation)

  protected trait ExprFactory {
    def withExpectedType(expectedType: TS#TType): Conv[TExprTypes#TExpr]
    def accessMember(memberName: MemberName, location: SourceLocation): ExprFactory = ???
    def forArguments(argInfo: ArgumentInfo): ExprFactory = ???
  }

  protected def exprFactory(f: TS#TType => Conv[TExprTypes#TExpr]): ExprFactory

  protected def wrapExpr(expr: ArExpr[TExprTypes]): TExprTypes#TExpr

  def convertStatements(env: Env, stmts: WithSource[Vector[WithSource[Stmt]]]): ExprFactory = stmts.value match {
    case Vector() =>
      fromFixedType(env, stmts.location)(
        wrapExpr(CreateTuple(Vector()))
      )

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

          variable = Variable(
            VariableDescriptor(env.owner, varId),
            name.map(VariableName.Normal).getOrElse(VariableName.Unnamed),
            Mutability.fromIsMutable(isMutable),
            varTypeResolved
          )

          tailExpr <- convertStatements(env.copy(scope = env.scope.addVariable(variable)), WithSource(tail, SourceLocation(location.end, stmts.location.end))).withExpectedType(expectedType)
        } yield wrapExpr(LetBinding(
          variable,
          valueExpr,
          tailExpr
        ))
      }

    case Vector(WithSource(expr: Expr, location)) =>
      convertExpression(env, WithSource(expr, location))

    case WithSource(expr: Expr, location) +: tail =>
      exprFactory { expectedType =>
        for {
          first <- convertExpression(env, WithSource(expr, location)).withExpectedType(typeComparer.typeBaseToType(TupleType(Vector())))
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

    case ClassConstructorExpr(classExpr) =>
      convertExpression(env, classExpr).accessMember(MemberName.New, expr.location)

    case DotExpr(baseExpr, memberName) =>
      convertExpression(env, baseExpr).accessMember(MemberName.Normal(memberName), expr.location)

    case FunctionCallExpr(func, arg) =>
      convertExpression(env, func).forArguments(ArgumentInfo(convertExpression(env, arg), expr.location))

    case IdentifierExpr(name) =>
      val idLookup = env.scope.findIdentifier(name, env.fileSpec, expr.location)
      val desc = LookupDescription.Identifier(name)

      new ScopeLookupExprFactory(desc, env, expr.location)(idLookup)

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

  protected def convertExpressionToType(expr: TExprTypes#TExpr): Conv[TS#TType]



  protected def fromFixedType(env: Env, location: SourceLocation)(expr: TExprTypes#TExpr): ExprFactory
  protected def fromFixedTypeConv(env: Env, location: SourceLocation)(expr: Conv[TExprTypes#TExpr]): ExprFactory
  protected def fromErrors(errors: CompilationMessage*): ExprFactory =
    new ExprFactory {
      override def withExpectedType(expectedType: TS#TType): Conv[TExprTypes#TExpr] =
        compilationInstance.forErrors(wrapExpr(InvalidExpression()), errors: _*)
    }

  protected def fromConv(factory: Conv[ExprFactory]): ExprFactory

  private abstract class LookupExprFactory[T](description: LookupDescription, env: Env, location: SourceLocation)(lookup: Lookup[T], cmp: LookupComparer[T]) extends ExprFactory {

    final override def withExpectedType(expectedType: TS#TType): Conv[TExprTypes#TExpr] =
      (lookup.resolve(cmp) match {
        case LookupResult.Failure(_) =>
          fromErrors(CompilationError.LookupFailedError(description, env.fileSpec, location))

        case LookupResult.Ambiguity(_, _, _, _) =>
          fromErrors(CompilationError.AmbiguousLookupError(description, env.fileSpec, location))

        case LookupResult.Success(result, _) =>
          handleResult(result)
      }).withExpectedType(expectedType)

    protected def handleResult(result: T): ExprFactory
  }

  private class ScopeLookupExprFactory
  (
    description: LookupDescription,
    env: Env,
    location: SourceLocation
  )(
    lookup: Lookup[ScopeValue[TScopeTypes]]
  ) extends LookupExprFactory[ScopeValue[TScopeTypes]](description, env, location)(lookup, scopeLookupComparer) {

    override protected def handleResult(result: ScopeValue[TScopeTypes]): ExprFactory =
      result match {
        case NamespaceScopeValue(_) =>
          fromErrors(CompilationError.NamespaceUsedAsValueError(description, env.fileSpec, location))

        case ClassScopeValue(arClass) =>
          ???

        case TraitScopeValue(arTrait) =>
          ???

        case DataConstructorScopeValue(ctor) =>
          ???

        case FunctionScopeValue(func) =>
          functionExprFactory(env, location)(func)

        case VariableScopeValue(variable) =>
          fromFixedType(env, location)(wrapExpr(LoadVariable[TExprTypes](variable)))
      }

  }

  protected def functionExprFactory(env: Env, location: SourceLocation)(func: TExprTypes#TFunction): ExprFactory
}

final case class ExpressionConvertEnvironment[Types <: ScopeTypes](owner: VariableOwnerDescriptor, scope: Scope[Types], fileSpec: FileSpec)


