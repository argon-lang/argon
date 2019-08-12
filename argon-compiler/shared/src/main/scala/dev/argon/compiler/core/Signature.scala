package dev.argon.compiler.core

import dev.argon.compiler._
import dev.argon.compiler.types._
import cats._
import cats.implicits._

trait SignatureContext
{
  val context: Context
  val typeSystem: TypeSystem[context.type]
  import typeSystem.Parameter

  sealed trait Signature[TResult[TContext2 <: Context with Singleton, _ <: TypeSystem[TContext2] with Singleton]] {

    def unsubstitutedParameters: Vector[Parameter]
    def unsubstitutedResult: TResult[context.type, typeSystem.type]

    def convertTypeSystem[F[_]: Monad](newContext: SignatureContext.Aux[context.type])(converter: TypeSystemConverter[context.type, typeSystem.type, newContext.typeSystem.type, F]): F[newContext.Signature[TResult]]

    def referencesParameter(parameter: Parameter): Boolean
    def substitute(parameter: Parameter)(replacement: typeSystem.WrapExpr): Signature[TResult]

    final def substituteTypeArguments(parameters: Vector[Parameter])(replacements: Vector[typeSystem.TypeArgument]): Signature[TResult] = {

      def handleNonReplaceableParam(sig: Signature[TResult])(param: typeSystem.Parameter): Signature[TResult] =
        if(!sig.referencesParameter(param))
          sig
        else
          ???

      parameters.zip(replacements).foldLeft(this) {
        case (sig, (param, typeSystem.TypeArgument.Expr(arg))) =>
          sig.substitute(param)(arg)

        case (sig, (param, typeSystem.TypeArgument.Wildcard(_))) =>
          handleNonReplaceableParam(sig)(param)
      }

    }

    def visit[A](fParam: SignatureParameters[TResult] => A, fResult: SignatureResult[TResult] => A): A

  }

  final case class SignatureParameters[TResult[TContext2 <: Context with Singleton, _ <: TypeSystem[TContext2] with Singleton]]
  (
    parameter: Parameter,
    nextUnsubstituted: Signature[TResult]
  ) extends Signature[TResult] {

    override def unsubstitutedParameters: Vector[Parameter] =
      parameter +: nextUnsubstituted.unsubstitutedParameters

    override def unsubstitutedResult: TResult[context.type, typeSystem.type] = nextUnsubstituted.unsubstitutedResult

    def next(expr: typeSystem.WrapExpr): Signature[TResult] =
      nextUnsubstituted.substitute(parameter)(expr)

    override def convertTypeSystem[F[_]: Monad](newContext: SignatureContext.Aux[context.type])(converter: TypeSystemConverter[context.type, typeSystem.type, newContext.typeSystem.type, F]): F[newContext.Signature[TResult]] =
      for {
        newParam <- TypeSystem.convertParameterTypeSystem(context)(typeSystem)(newContext.typeSystem)(converter)(parameter)
        newNext <- nextUnsubstituted.convertTypeSystem(newContext)(converter)
      } yield newContext.SignatureParameters(newParam, newNext)

    override def referencesParameter(parameter: typeSystem.Parameter): Boolean =
      new RefChecker(parameter).checkVariable(this.parameter.paramVar)

    override def substitute(parameter: typeSystem.Parameter)(replacement: typeSystem.WrapExpr): Signature[TResult] =
      SignatureParameters(
        Substitutions(typeSystem)(parameter.paramVar, replacement).substParameter(this.parameter),
        nextUnsubstituted.substitute(parameter)(replacement)
      )

    override def visit[A](fParam: SignatureParameters[TResult] => A, fResult: SignatureResult[TResult] => A): A = fParam(this)
  }

  final case class SignatureResult[TResult[TContext2 <: Context with Singleton, _ <: TypeSystem[TContext2] with Singleton] : SignatureResultConverter]
  (
    result: TResult[context.type, typeSystem.type]
  ) extends Signature[TResult] {

    override def unsubstitutedParameters: Vector[Parameter] = Vector.empty

    override def unsubstitutedResult: TResult[context.type, typeSystem.type] = result

    override def convertTypeSystem[F[_]: Monad](newContext: SignatureContext.Aux[context.type])(converter: TypeSystemConverter[context.type, typeSystem.type, newContext.typeSystem.type, F]): F[newContext.Signature[TResult]] =
      for {
        newResult <- implicitly[SignatureResultConverter[TResult]].convertTypeSystem(context)(typeSystem)(newContext.typeSystem)(converter)(result)
      } yield newContext.SignatureResult(newResult)

    override def referencesParameter(parameter: typeSystem.Parameter): Boolean =
      implicitly[SignatureResultConverter[TResult]].referencesParameter(SignatureContext.this)(new RefChecker(parameter))(result)

    override def substitute(parameter: typeSystem.Parameter)(replacement: typeSystem.WrapExpr): Signature[TResult] =
      SignatureResult(implicitly[SignatureResultConverter[TResult]].substitute(SignatureContext.this)(Substitutions(typeSystem)(parameter.paramVar, replacement))(result))

    override def visit[A](fParam: SignatureParameters[TResult] => A, fResult: SignatureResult[TResult] => A): A = fResult(this)
  }

  type Subst = Substitutions.Aux[context.type, typeSystem.type]

  class RefChecker private[SignatureContext](parameter: Parameter) {

    import typeSystem._

    def checkTypeArg(arg: TypeArgument): Boolean = arg match {
      case TypeArgument.Expr(argExpr) => checkWrapExpr(argExpr)
      case TypeArgument.Wildcard(_) => false
    }

    def checkVariable(variable: Variable): Boolean =
      checkWrapExpr(variable.varType)

    def checkArExpr(expr: ArExpr): Boolean =
      expr match {
        case ClassConstructorCall(classType, _, args) =>
          checkArExpr(classType) || args.exists(checkWrapExpr)

        case DataConstructorCall(dataCtorInstanceType, args) =>
          checkArExpr(dataCtorInstanceType) || args.exists(checkWrapExpr)

        case FunctionCall(_, args, returnType) =>
          args.exists(checkWrapExpr) || checkWrapExpr(returnType)

        case FunctionObjectCall(function, arg, returnType) =>
          checkWrapExpr(function) || checkWrapExpr(arg) || checkWrapExpr(returnType)

        case IfElse(condition, ifBody, elseBody) =>
          checkWrapExpr(condition) || checkWrapExpr(ifBody) || checkWrapExpr(elseBody)

        case LetBinding(variable, value, next) =>
          checkVariable(variable) || checkWrapExpr(value) || checkWrapExpr(next)

        case LoadConstantBool(_, _) => false
        case LoadConstantInt(_, _) => false
        case LoadConstantString(_, _) => false
        case LoadLambda(argVariable, body) => checkVariable(argVariable) || checkWrapExpr(body)
        case LoadTuple(values) => values.exists { case TupleElement(value) => checkWrapExpr(value) }
        case LoadUnit(_) => false
        case LoadVariable(variable) if variable.descriptor === parameter.paramVar.descriptor => true
        case LoadVariable(variable) => checkVariable(variable)
        case MethodCall(_, instance, args, returnType) =>
          checkWrapExpr(instance) || args.exists(checkWrapExpr) || checkWrapExpr(returnType)

        case PrimitiveOp(_, left, right, exprType) =>
          checkWrapExpr(left) || checkWrapExpr(right) || checkWrapExpr(exprType)

        case Sequence(first, second) =>
          checkWrapExpr(first) || checkWrapExpr(first)

        case StoreVariable(variable, value, exprType) =>
          checkVariable(variable) || checkWrapExpr(value) || checkWrapExpr(exprType)

        case TraitType(_, args, baseTypes) =>
          args.exists(checkTypeArg) ||
            baseTypes.baseTraits.exists(checkArExpr)

        case ClassType(_, args, baseTypes) =>
          args.exists(checkTypeArg) ||
            baseTypes.baseClass.exists(checkArExpr) ||
            baseTypes.baseTraits.exists(checkArExpr)

        case DataConstructorType(_, args, instanceType) =>
          args.exists(checkTypeArg) || checkArExpr(instanceType)

        case TypeOfType(inner) => checkWrapExpr(inner)
        case TypeN(_, subtypeConstraint, supertypeConstraint) =>
          subtypeConstraint.exists(checkWrapExpr) || supertypeConstraint.exists(checkWrapExpr)

        case FunctionType(argumentType, resultType) => checkWrapExpr(argumentType) || checkWrapExpr(resultType)
        case UnionType(first, second) => checkWrapExpr(first) || checkWrapExpr(second)
        case IntersectionType(first, second) => checkWrapExpr(first) || checkWrapExpr(second)

      }

    def checkWrapExpr(expr: TType): Boolean =
      traverseTypeWrapper(expr) { t =>
        if(checkArExpr(t)) Some(()) else None
      }.isDefined
  }

}

object SignatureContext {
  type Aux[TContext <: Context with Singleton] = SignatureContext { val context: TContext }
}
