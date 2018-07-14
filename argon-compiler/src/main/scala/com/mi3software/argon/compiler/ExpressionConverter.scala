package com.mi3software.argon.compiler

import com.mi3software.argon.parser._
import com.mi3software.argon.util.{FileSpec, SourceLocation, WithSource}
import com.mi3software.argon.Compilation

import scalaz._
import Scalaz._
import ScopeHelpers._

trait ExpressionConverter {

  type Conv[T]
  protected implicit val monadInstance: Monad[Conv]
  protected implicit val compilationInstance: Compilation[Conv]

  protected def nextVariableId: Conv[Int]

  val context: Context

  val exprTypes: ArExprTypes with ({
    type TFunction = ArFunc[context.type]
    type TMethod = ArMethod[context.type]
    type TClassConstructor = ClassConstructor[context.type]
  })
  type TS = exprTypes.TS

  type TScopeTypes <: ScopeTypes with ({
    type TTrait = ArTrait[context.type]
    type TClass = ArClass[context.type]
    type TDataConstructor = DataConstructor[context.type]
    type TFunc = ArFunc[context.type]
    type TVariable = Variable[TS, VariableLikeDescriptor]
  })

  protected val contextTypeSystemConverter: TypeSystemConverter[context.typeSystem.type, TS]

  val typeComparer: TypeComparer[TS]

  val scopeLookupComparer: LookupComparer[ScopeValue[TScopeTypes]]

  protected def createTypeHole: Conv[TS#TType]
  protected def resolveType(t: TS#TType): Conv[TS#TType]

  type Env = ExpressionConvertEnvironment[TScopeTypes]

  protected sealed case class ArgumentInfo(argFactory: ExprFactory, location: SourceLocation)

  protected trait ExprFactory {
    def withExpectedType(expectedType: TS#TType): Conv[exprTypes.TExpr]
    def accessMember(memberName: MemberName, location: SourceLocation): ExprFactory = ???
    def forArguments(argInfo: ArgumentInfo): ExprFactory = ???
  }

  protected def exprFactory(f: TS#TType => Conv[exprTypes.TExpr]): ExprFactory

  protected def wrapExpr(expr: ArExpr[exprTypes.type]): exprTypes.TExpr

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

          variable = Variable[exprTypes.typeSystem.type, VariableDescriptor](
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
        } yield wrapExpr(LoadTypeValue[exprTypes.type](typeComparer.typeBaseToType(FunctionType[TS](argType, resultType))))
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

  protected def convertExpressionToType(expr: exprTypes.TExpr): Conv[TS#TType]



  protected def fromFixedType(env: Env, location: SourceLocation)(expr: exprTypes.TExpr): ExprFactory
  protected def fromFixedTypeConv(env: Env, location: SourceLocation)(expr: Conv[exprTypes.TExpr]): ExprFactory
  protected def fromErrors(errors: CompilationMessage*): ExprFactory =
    new ExprFactory {
      override def withExpectedType(expectedType: TS#TType): Conv[exprTypes.TExpr] =
        compilationInstance.forErrors(wrapExpr(InvalidExpression()), errors: _*)
    }

  protected def fromConv(factory: Conv[ExprFactory]): ExprFactory

  private abstract class LookupExprFactory[T](description: LookupDescription, env: Env, location: SourceLocation)(lookup: Lookup[T], cmp: LookupComparer[T]) extends ExprFactory {

    final override def withExpectedType(expectedType: TS#TType): Conv[exprTypes.TExpr] =
      (lookup.resolve(cmp) match {
        case LookupResult.Failure(_) =>
          fromErrors(CompilationError.LookupFailedError(description, CompilationMessageSource.SourceFile(env.fileSpec, location)))

        case LookupResult.Ambiguity(_, _, _, _) =>
          fromErrors(CompilationError.AmbiguousLookupError(description, CompilationMessageSource.SourceFile(env.fileSpec, location)))

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
          fromErrors(CompilationError.NamespaceUsedAsValueError(description, CompilationMessageSource.SourceFile(env.fileSpec, location)))

        case ClassScopeValue(arClass) =>
          ???

        case TraitScopeValue(arTrait) =>
          ???

        case DataConstructorScopeValue(ctor) =>
          ???

        case FunctionScopeValue(func) =>
          functionExprFactory(env, location)(func)

        case VariableScopeValue(variable) =>
          fromFixedType(env, location)(wrapExpr(LoadVariable[exprTypes.type](variable)))
      }

  }

  protected final def functionExprFactory(env: Env, location: SourceLocation)(func: exprTypes.TFunction): ExprFactory = {
    final class FunctionExprFactory(env: Env, location: SourceLocation)(func: exprTypes.TFunction, args: Vector[exprTypes.TExpr], signature: Signature[TS, FunctionResultInfo]) extends ExprFactory {


      override def forArguments(argInfo: ArgumentInfo): ExprFactory =
        signature match {
          case parameters: SignatureParameters[TS, FunctionResultInfo] =>
            fromConv(
              argInfo.argFactory.withExpectedType(Parameter.paramType(exprTypes.typeSystem)(parameters.parameter)).flatMap { param =>
                convertExpressionToType(param)
                  .flatMap { paramAsType =>
                    parameters.next(paramAsType)
                  }
                  .map { sigNext =>
                    new FunctionExprFactory(env, argInfo.location)(func, args :+ param, sigNext)
                  }
              }
            )

          case result: SignatureResult[TS, FunctionResultInfo] =>
            fromFixedType(env, location)(
              wrapExpr(FunctionCall[exprTypes.type](func, args, result.result.returnType))
            ).forArguments(argInfo)
        }

      override def withExpectedType(expectedType: TS#TType): Conv[exprTypes.TExpr] =
        signature match {
          case parameters: SignatureParameters[TS, FunctionResultInfo] =>
            ???

          case result: SignatureResult[TS, FunctionResultInfo] =>
            fromFixedType(env, location)(
              wrapExpr(FunctionCall[exprTypes.type](func, args, result.result.returnType))
            ).withExpectedType(expectedType)
        }
    }

    new FunctionExprFactory(env, location)(func, Vector(), func.signature.convertTypeSystem(contextTypeSystemConverter))
  }



}

final case class ExpressionConvertEnvironment[Types <: ScopeTypes](owner: VariableOwnerDescriptor, scope: Scope[Types], fileSpec: FileSpec)


