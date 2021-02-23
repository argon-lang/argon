package dev.argon.compiler.core

import dev.argon.compiler._
import dev.argon.compiler.types._
import cats._
import cats.evidence.===
import cats.implicits._
import dev.argon.compiler.expr._
import dev.argon.compiler.expr.ArExpr._
import shapeless.ops.nat.{LT, Pred, ToInt}
import shapeless.{Nat, Sized, Succ, _0}
import zio.{IO, ZIO}

trait SignatureContext {
  val context: Context
  type TTypeWrapper[+_]
  implicit val typeWrapperInstances: WrapperInstance[TTypeWrapper]

  private type TParameter = dev.argon.compiler.expr.Parameter[context.type, TTypeWrapper]

  trait SignatureVisitor[TResult[TContext2 <: Context with Singleton, Wrap[+_]], Len <: Nat, A] {
    def visitParameters[RestLen <: Nat](sigParams: SignatureParameters[TResult, RestLen])(implicit lenPred: Pred.Aux[Len, RestLen], lenPositive: LT[_0, Len]): A
    def visitResult(sigResult: SignatureResult[TResult])(implicit lenEq: Len === _0): A
  }

  sealed trait Signature[TResult[TContext2 <: Context with Singleton, Wrap[+_]], Len <: Nat] {

    val parameterCount: Len
    implicit val parameterCountToInt: ToInt[Len]

    def unsubstitutedParameters: Sized[Vector[TParameter], Len]
    def unsubstitutedResult: TResult[context.type, TTypeWrapper]

    def convertTypeSystem(newContext: SignatureContext.Aux[context.type])(converter: TypeSystemConverter.Aux[context.type, TTypeWrapper, newContext.TTypeWrapper]): Comp[newContext.Signature[TResult, Len]]

    def referencesParameter(parameter: TParameter): Comp[Boolean]
    def substitute(parameter: TParameter)(replacement: ArExprWrap[context.type, TTypeWrapper]): Signature[TResult, Len]

    final def substituteTypeArguments(parameters: Vector[TParameter])(replacements: Vector[ArExprWrap[context.type, TTypeWrapper]]): Signature[TResult, Len] =
      parameters.zip(replacements).foldLeft(this) {
        case (sig, (param, arg)) =>
          sig.substitute(param)(arg)
      }

    def visit[A](visitor: SignatureVisitor[TResult, Len, A]): A
    def toSignatureParameters[RestLen <: Nat](implicit pred: Pred.Aux[Len, RestLen]): SignatureParameters[TResult, RestLen]
  }

  final case class SignatureParameters[TResult[TContext2 <: Context with Singleton, Wrap[+_]], RestLen <: Nat]
  (
    parameter: TParameter,
    nextUnsubstituted: Signature[TResult, RestLen]
  ) extends Signature[TResult, Succ[RestLen]] {

    type Len = Succ[RestLen]

    override val parameterCount: Len = Succ()
    override implicit val parameterCountToInt: ToInt[Succ[RestLen]] =
      ToInt.toIntSucc(nextUnsubstituted.parameterCountToInt)

    override def unsubstitutedParameters: Sized[Vector[TParameter], Len] =
      parameter +: nextUnsubstituted.unsubstitutedParameters

    override def unsubstitutedResult: TResult[context.type, TTypeWrapper] = nextUnsubstituted.unsubstitutedResult

    def next(expr: ArExprWrap[context.type, TTypeWrapper]): Signature[TResult, RestLen] =
      nextUnsubstituted.substitute(parameter)(expr)

    override def convertTypeSystem(newContext: SignatureContext.Aux[context.type])(converter: TypeSystemConverter.Aux[context.type, TTypeWrapper, newContext.TTypeWrapper]): Comp[newContext.Signature[TResult, Len]] =
      for {
        newParam <- converter.convertParameterTypeSystem(parameter)
        newNext <- nextUnsubstituted.convertTypeSystem(newContext)(converter)
      } yield newContext.SignatureParameters(newParam, newNext)

    override def referencesParameter(parameter: TParameter): Comp[Boolean] =
      IO.succeed(new RefChecker(parameter).checkVariable(this.parameter.paramVar))

    override def substitute(parameter: TParameter)(replacement: ArExprWrap[context.type, TTypeWrapper]): Signature[TResult, Len] =
      SignatureParameters(
        Substitutions(context)(parameter.paramVar, replacement).substParameter(this.parameter),
        nextUnsubstituted.substitute(parameter)(replacement)
      )

    override def visit[A](visitor: SignatureVisitor[TResult, Succ[RestLen], A]): A = visitor.visitParameters(this)

    @SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
    override def toSignatureParameters[RestLen2 <: Nat](implicit pred: Pred.Aux[Succ[RestLen], RestLen2]): SignatureParameters[TResult, RestLen2] =
      this.asInstanceOf[SignatureParameters[TResult, RestLen2]]



  }

  final case class SignatureResult[TResult[TContext2 <: Context with Singleton, Wrap[+_]] : SignatureResultConverter]
  (
    result: TResult[context.type, TTypeWrapper]
  ) extends Signature[TResult, _0] {

    override val parameterCount: _0 = Nat._0
    override val parameterCountToInt: ToInt[_0] = implicitly

    override def unsubstitutedParameters: Sized[Vector[TParameter], _0] = Sized[Vector]()

    override def unsubstitutedResult: TResult[context.type, TTypeWrapper] = result

    override def convertTypeSystem(newContext: SignatureContext.Aux[context.type])(converter: TypeSystemConverter.Aux[context.type, TTypeWrapper, newContext.TTypeWrapper]): Comp[newContext.Signature[TResult, _0]] =
      for {
        newResult <- implicitly[SignatureResultConverter[TResult]].convertTypeSystem(context)(converter)(result)
      } yield newContext.SignatureResult(newResult)

    override def referencesParameter(parameter: TParameter): Comp[Boolean] =
      implicitly[SignatureResultConverter[TResult]].referencesParameter(SignatureContext.this)(new RefChecker(parameter))(result)

    override def substitute(parameter: TParameter)(replacement: ArExprWrap[context.type, TTypeWrapper]): Signature[TResult, _0] =
      SignatureResult(implicitly[SignatureResultConverter[TResult]].substitute(SignatureContext.this)(Substitutions(context)(parameter.paramVar, replacement))(result))

    override def visit[A](visitor: SignatureVisitor[TResult, _0, A]): A = visitor.visitResult(this)

    @SuppressWarnings(Array("org.wartremover.warts.Throw"))
    override def toSignatureParameters[RestLen <: Nat](implicit pred: Pred.Aux[_0, RestLen]): SignatureParameters[TResult, RestLen] =
      throw new UnsupportedOperationException()
  }

  type Subst = Substitutions.Aux[context.type, TTypeWrapper]

  class RefChecker private[SignatureContext](parameter: TParameter) {

    def checkVariable(variable: Variable[context.type, TTypeWrapper]): Boolean =
      checkWrapExpr(variable.varType)

    def checkArExpr(expr: ArExpr[context.type, TTypeWrapper]): Boolean =
      expr match {
        case ClassConstructorCall(classType, _, args) =>
          checkArExpr(classType) || args.exists(checkWrapExpr)

        case DataConstructorCall(dataCtorInstanceType, args) =>
          checkArExpr(dataCtorInstanceType) || args.exists(checkWrapExpr)

        case EnsureExecuted(body, ensuring) =>
          checkWrapExpr(body) || checkWrapExpr(ensuring)

        case FunctionCall(_, args, _) =>
          args.exists(checkWrapExpr)

        case FunctionObjectCall(function, arg, _) =>
          checkWrapExpr(function) || checkWrapExpr(arg)

        case IfElse(condition, ifBody, elseBody) =>
          checkWrapExpr(condition) || checkWrapExpr(ifBody) || checkWrapExpr(elseBody)

        case LetBinding(variable, value, next) =>
          checkVariable(variable) || checkWrapExpr(value) || checkWrapExpr(next)

        case LoadConstantBool(_, _) => false
        case LoadConstantInt(_, _) => false
        case LoadConstantString(_, _) => false
        case LoadLambda(argVariable, body) => checkVariable(argVariable) || checkWrapExpr(body)
        case LoadTuple(values) => values.exists { case TupleElement(value) => checkWrapExpr(value) }
        case LoadTupleElement(value, _, _) => checkWrapExpr(value)
        case LoadUnit(_) => false
        case LoadVariable(variable) if variable === parameter.paramVar => true
        case LoadVariable(variable) => checkVariable(variable)
        case MethodCall(_, instance, _, args, _) =>
          checkWrapExpr(instance) || args.exists(checkWrapExpr)

        case PatternMatch(expr, cases) =>
          checkWrapExpr(expr) || cases.exists { case PatternCase(pattern, body) => checkPatternExpr(pattern) || checkWrapExpr(body) }

        case Sequence(first, second) =>
          checkWrapExpr(first) || checkWrapExpr(second)

        case StoreVariable(variable, value, exprType) =>
          checkVariable(variable) || checkWrapExpr(value) || checkWrapExpr(exprType)

        case TraitType(_, args) =>
          args.exists(checkWrapExpr)

        case ClassType(_, args) =>
          args.exists(checkWrapExpr)

        case DataConstructorType(_, args, _) =>
          args.exists(checkWrapExpr)

        case TypeOfType(inner) => checkWrapExpr(inner)
        case TypeN(_, subtypeConstraint, supertypeConstraint) =>
          subtypeConstraint.exists(checkWrapExpr) || supertypeConstraint.exists(checkWrapExpr)

        case FunctionType(argumentType, resultType) => checkWrapExpr(argumentType) || checkWrapExpr(resultType)
        case UnionType(first, second) => checkWrapExpr(first) || checkWrapExpr(second)
        case IntersectionType(first, second) => checkWrapExpr(first) || checkWrapExpr(second)
        case ExistentialType(variable, body) => checkVariable(variable) || checkWrapExpr(body)

      }

    def checkPatternExpr(expr: PatternExpr[context.type, TTypeWrapper]): Boolean =
      expr match {
        case PatternExpr.DataDeconstructor(_, args) => args.exists(checkPatternExpr)
        case PatternExpr.Binding(variable) => checkVariable(variable)
        case PatternExpr.CastBinding(variable) => checkVariable(variable)
      }

    def checkWrapExpr(expr: ArExprWrap[context.type, TTypeWrapper]): Boolean =
      expr.traverse { t =>
        if(checkArExpr(t)) Some(()) else None
      }.isDefined
  }

}

object SignatureContext {
  type Aux[TContext <: Context with Singleton] = SignatureContext { val context: TContext }
  type Aux2[TContext <: Context with Singleton, Wrap[+_]] = SignatureContext { val context: TContext; type TTypeWrapper[+A] = Wrap[A] }

  def liftSignatureResult[TResult[TContext2 <: Context with Singleton, _[+_]], Wrap[+_]: WrapperInstance](context: Context)(sig: context.signatureContext.Signature[TResult, _ <: Nat], args: Vector[ArExprWrap[context.type, Wrap]]): Comp[TResult[context.type, Wrap]] = {
    val ctx: context.type = context
    val conv = ArTypeSystemConverter[Wrap](context)

    val sigContext = new SignatureContext {
      override val context: ctx.type = ctx
      override type TTypeWrapper[+A] = Wrap[A]
      override val typeWrapperInstances: WrapperInstance[TTypeWrapper] = implicitly
    }

    for {
      convSig <- sig.convertTypeSystem(sigContext)(conv)
      substSig = convSig.substituteTypeArguments(convSig.unsubstitutedParameters)(args)
    } yield substSig.unsubstitutedResult
  }

}
