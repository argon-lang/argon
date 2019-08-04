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
    def substitute(parameter: Parameter)(replacement: typeSystem.ArExpr): Signature[TResult]

    final def substituteTypeArguments(parameters: Vector[Parameter])(replacements: Vector[typeSystem.TypeArgument]): Signature[TResult] = {

      def handleNonReplaceableParam(sig: Signature[TResult])(param: typeSystem.Parameter): Signature[TResult] =
        if(!sig.referencesParameter(param))
          sig
        else
          ???

      parameters.zip(replacements).foldLeft(this) {
        case (sig, (param, typeSystem.TypeArgument.Expr(arg))) =>
          typeSystem.unwrapType(arg) match {
            case Some(arg) => sig.substitute(param)(arg)
            case None => handleNonReplaceableParam(sig)(param)
          }

        case (sig, (param, typeSystem.TypeArgument.Wildcard)) =>
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

    def next(expr: typeSystem.ArExpr): Signature[TResult] =
      nextUnsubstituted.substitute(parameter)(expr)

    override def convertTypeSystem[F[_]: Monad](newContext: SignatureContext.Aux[context.type])(converter: TypeSystemConverter[context.type, typeSystem.type, newContext.typeSystem.type, F]): F[newContext.Signature[TResult]] =
      for {
        newParam <- TypeSystem.convertParameterTypeSystem(context)(typeSystem)(newContext.typeSystem)(converter)(parameter)
        newNext <- nextUnsubstituted.convertTypeSystem(newContext)(converter)
      } yield newContext.SignatureParameters(newParam, newNext)

    override def referencesParameter(parameter: typeSystem.Parameter): Boolean =
      new RefChecker(parameter).checkVariable(this.parameter.paramVar)

    override def substitute(parameter: typeSystem.Parameter)(replacement: typeSystem.ArExpr): Signature[TResult] =
      SignatureParameters(
        new Substitutions(parameter, replacement).substParameter(this.parameter),
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

    override def substitute(parameter: typeSystem.Parameter)(replacement: typeSystem.ArExpr): Signature[TResult] =
      SignatureResult(implicitly[SignatureResultConverter[TResult]].substitute(SignatureContext.this)(new Substitutions(parameter, replacement))(result))

    override def visit[A](fParam: SignatureParameters[TResult] => A, fResult: SignatureResult[TResult] => A): A = fResult(this)
  }

  class Substitutions private[SignatureContext](parameter: Parameter, replacement: typeSystem.ArExpr) {

    import typeSystem._

    def substTypeArg(arg: TypeArgument): TypeArgument = arg match {
      case TypeArgument.Expr(argExpr) => TypeArgument.Expr(substWrapExpr(argExpr))
      case TypeArgument.Wildcard => TypeArgument.Wildcard
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
        case LoadVariable(variable) if variable.descriptor === parameter.paramVar.descriptor => replacement
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
        case TypeOfType(inner, universe) => TypeOfType(substWrapExpr(inner), universe)
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

  class RefChecker private[SignatureContext](parameter: Parameter) {

    import typeSystem._

    def checkTypeArg(arg: TypeArgument): Boolean = arg match {
      case TypeArgument.Expr(argExpr) => checkWrapExpr(argExpr)
      case TypeArgument.Wildcard => false
    }

    def checkVariable(variable: Variable): Boolean =
      checkWrapExpr(variable.varType)

    def checkArExpr(expr: ArExpr): Boolean =
      expr match {
        case ClassConstructorCall(classType, _, args) =>
          checkArExpr(classType) || args.exists(checkArExpr)

        case DataConstructorCall(dataCtorInstanceType, args) =>
          checkArExpr(dataCtorInstanceType) || args.exists(checkArExpr)

        case FunctionCall(_, args, returnType) =>
          args.exists(checkArExpr) || checkWrapExpr(returnType)

        case FunctionObjectCall(function, arg, returnType) =>
          checkArExpr(function) || checkArExpr(arg) || checkWrapExpr(returnType)

        case IfElse(condition, ifBody, elseBody) =>
          checkArExpr(condition) || checkArExpr(ifBody) || checkArExpr(elseBody)

        case LetBinding(variable, value, next) =>
          checkVariable(variable) || checkArExpr(value) || checkArExpr(next)

        case LoadConstantBool(_, _) => false
        case LoadConstantInt(_, _) => false
        case LoadConstantString(_, _) => false
        case LoadLambda(argVariable, body) => checkVariable(argVariable) || checkArExpr(body)
        case LoadTuple(values) => values.exists { case TupleElement(value) => checkWrapExpr(value) }
        case LoadUnit(_) => false
        case LoadVariable(variable) if variable.descriptor === parameter.paramVar.descriptor => true
        case LoadVariable(variable) => checkVariable(variable)
        case MethodCall(_, instance, args, returnType) =>
          checkArExpr(instance) || args.exists(checkArExpr) || checkWrapExpr(returnType)

        case PrimitiveOp(_, left, right, exprType) =>
          checkArExpr(left) || checkArExpr(right) || checkWrapExpr(exprType)

        case Sequence(first, second) =>
          checkArExpr(first) || checkArExpr(first)

        case StoreVariable(variable, value, exprType) =>
          checkVariable(variable) || checkArExpr(value) || checkWrapExpr(exprType)

        case TraitType(_, args, baseTypes) =>
          args.exists(checkTypeArg) ||
            baseTypes.baseTraits.exists(checkArExpr)

        case ClassType(_, args, baseTypes) =>
          args.exists(checkTypeArg) ||
            baseTypes.baseClass.exists(checkArExpr) ||
            baseTypes.baseTraits.exists(checkArExpr)

        case DataConstructorType(_, args, instanceType) =>
          args.exists(checkTypeArg) || checkArExpr(instanceType)

        case TypeOfType(inner, _) => checkWrapExpr(inner)
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
