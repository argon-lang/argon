package dev.argon.compiler.core

import dev.argon.compiler._
import dev.argon.compiler.types._
import cats._
import cats.evidence.===
import cats.implicits._
import shapeless.ops.nat.{LT, Pred, ToInt}
import shapeless.{Nat, Sized, Succ, _0}

trait SignatureContext
{
  val context: Context
  val typeSystem: TypeSystem[context.type]
  import typeSystem.Parameter

  trait SignatureVisitor[TResult[TContext2 <: Context with Singleton, _ <: TypeSystem[TContext2] with Singleton], Len <: Nat, A] {
    def visitParameters[RestLen <: Nat](sigParams: SignatureParameters[TResult, RestLen])(implicit lenPred: Pred.Aux[Len, RestLen], lenPositive: LT[_0, Len]): A
    def visitResult(sigResult: SignatureResult[TResult])(implicit lenEq: Len === _0): A
  }

  sealed trait Signature[TResult[TContext2 <: Context with Singleton, _ <: TypeSystem[TContext2] with Singleton], Len <: Nat] {

    val parameterCount: Len
    implicit val parameterCountToInt: ToInt[Len]

    def unsubstitutedParameters: Sized[Vector[Parameter], Len]
    def unsubstitutedResult: TResult[context.type, typeSystem.type]

    def convertTypeSystem[F[_]: Monad](newContext: SignatureContext.Aux[context.type])(converter: TypeSystemConverter[context.type, typeSystem.type, newContext.typeSystem.type, F]): F[newContext.Signature[TResult, Len]]

    def referencesParameter(parameter: Parameter): Boolean
    def substitute(parameter: Parameter)(replacement: typeSystem.WrapExpr): Signature[TResult, Len]

    final def substituteTypeArguments(parameters: Vector[Parameter])(replacements: Vector[typeSystem.TypeArgument]): Signature[TResult, Len] = {

      def handleNonReplaceableParam(sig: Signature[TResult, Len])(param: typeSystem.Parameter): Signature[TResult, Len] =
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

    def visit[A](visitor: SignatureVisitor[TResult, Len, A]): A
    def toSignatureParameters[RestLen <: Nat](implicit pred: Pred.Aux[Len, RestLen]): SignatureParameters[TResult, RestLen]
  }

  final case class SignatureParameters[TResult[TContext2 <: Context with Singleton, _ <: TypeSystem[TContext2] with Singleton], RestLen <: Nat]
  (
    parameter: Parameter,
    nextUnsubstituted: Signature[TResult, RestLen]
  ) extends Signature[TResult, Succ[RestLen]] {

    type Len = Succ[RestLen]

    override val parameterCount: Len = Succ()
    override implicit val parameterCountToInt: ToInt[Succ[RestLen]] =
      ToInt.toIntSucc(nextUnsubstituted.parameterCountToInt)

    override def unsubstitutedParameters: Sized[Vector[Parameter], Len] =
      parameter +: nextUnsubstituted.unsubstitutedParameters

    override def unsubstitutedResult: TResult[context.type, typeSystem.type] = nextUnsubstituted.unsubstitutedResult

    def next(expr: typeSystem.WrapExpr): Signature[TResult, RestLen] =
      nextUnsubstituted.substitute(parameter)(expr)

    override def convertTypeSystem[F[_]: Monad](newContext: SignatureContext.Aux[context.type])(converter: TypeSystemConverter[context.type, typeSystem.type, newContext.typeSystem.type, F]): F[newContext.Signature[TResult, Len]] =
      for {
        newParam <- TypeSystem.convertParameterTypeSystem(context)(typeSystem)(newContext.typeSystem)(converter)(parameter)
        newNext <- nextUnsubstituted.convertTypeSystem(newContext)(converter)
      } yield newContext.SignatureParameters(newParam, newNext)

    override def referencesParameter(parameter: typeSystem.Parameter): Boolean =
      new RefChecker(parameter).checkVariable(this.parameter.paramVar)

    override def substitute(parameter: typeSystem.Parameter)(replacement: typeSystem.WrapExpr): Signature[TResult, Len] =
      SignatureParameters(
        Substitutions(typeSystem)(parameter.paramVar, replacement).substParameter(this.parameter),
        nextUnsubstituted.substitute(parameter)(replacement)
      )

    override def visit[A](visitor: SignatureVisitor[TResult, Succ[RestLen], A]): A = visitor.visitParameters(this)

    @SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
    override def toSignatureParameters[RestLen2 <: Nat](implicit pred: Pred.Aux[Succ[RestLen], RestLen2]): SignatureParameters[TResult, RestLen2] =
      this.asInstanceOf[SignatureParameters[TResult, RestLen2]]



  }

  final case class SignatureResult[TResult[TContext2 <: Context with Singleton, _ <: TypeSystem[TContext2] with Singleton] : SignatureResultConverter]
  (
    result: TResult[context.type, typeSystem.type]
  ) extends Signature[TResult, _0] {

    override val parameterCount: _0 = Nat._0
    override val parameterCountToInt: ToInt[_0] = implicitly

    override def unsubstitutedParameters: Sized[Vector[Parameter], _0] = Sized[Vector]()

    override def unsubstitutedResult: TResult[context.type, typeSystem.type] = result

    override def convertTypeSystem[F[_]: Monad](newContext: SignatureContext.Aux[context.type])(converter: TypeSystemConverter[context.type, typeSystem.type, newContext.typeSystem.type, F]): F[newContext.Signature[TResult, _0]] =
      for {
        newResult <- implicitly[SignatureResultConverter[TResult]].convertTypeSystem(context)(typeSystem)(newContext.typeSystem)(converter)(result)
      } yield newContext.SignatureResult(newResult)

    override def referencesParameter(parameter: typeSystem.Parameter): Boolean =
      implicitly[SignatureResultConverter[TResult]].referencesParameter(SignatureContext.this)(new RefChecker(parameter))(result)

    override def substitute(parameter: typeSystem.Parameter)(replacement: typeSystem.WrapExpr): Signature[TResult, _0] =
      SignatureResult(implicitly[SignatureResultConverter[TResult]].substitute(SignatureContext.this)(Substitutions(typeSystem)(parameter.paramVar, replacement))(result))

    override def visit[A](visitor: SignatureVisitor[TResult, _0, A]): A = visitor.visitResult(this)

    @SuppressWarnings(Array("org.wartremover.warts.Throw"))
    override def toSignatureParameters[RestLen <: Nat](implicit pred: Pred.Aux[_0, RestLen]): SignatureParameters[TResult, RestLen] =
      throw new UnsupportedOperationException()
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
