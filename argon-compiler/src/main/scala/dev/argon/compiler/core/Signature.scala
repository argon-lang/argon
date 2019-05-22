package dev.argon.compiler.core

import dev.argon.compiler._
import dev.argon.compiler.types._
import cats._
import cats.implicits._

trait SignatureContext[TContext <: Context with Singleton]
{
  val context: TContext
  val typeSystem: TypeSystem[context.type]
  import typeSystem.Parameter

  sealed trait Signature[TResult[TContext2 <: Context with Singleton, _ <: TypeSystem[TContext2] with Singleton]] {

    def unsubstitutedParameters: Vector[Parameter]
    def unsubstitutedResult: TResult[context.type, typeSystem.type]

    def convertTypeSystem[F[_]: Monad](newContext: SignatureContext[context.type])(converter: TypeSystemConverter[context.type, typeSystem.type, newContext.typeSystem.type, F]): F[newContext.Signature[TResult]]
    def mapResult[TNewResult[TContext2 <: Context with Singleton, _ <: TypeSystem[TContext2]]](f: TResult[context.type, typeSystem.type] => TNewResult[context.type, typeSystem.type]): Signature[TNewResult]

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

    def next[Comp[_] : Compilation](param: typeSystem.ArExpr): Comp[Signature[TResult]] =
      nextUnsubstituted.pure[Comp]

    override def convertTypeSystem[F[_]: Monad](newContext: SignatureContext[context.type])(converter: TypeSystemConverter[context.type, typeSystem.type, newContext.typeSystem.type, F]): F[newContext.Signature[TResult]] =
      for {
        newParam <- TypeSystem.convertParameterTypeSystem(context)(typeSystem)(newContext.typeSystem)(converter)(parameter)
        newNext <- nextUnsubstituted.convertTypeSystem(newContext)(converter)
      } yield newContext.SignatureParameters(newParam, newNext)

    override def mapResult[TNewResult[TContext2 <: Context with Singleton, _ <: TypeSystem[TContext2]]](f: TResult[context.type, typeSystem.type] => TNewResult[context.type, typeSystem.type]): Signature[TNewResult] = ???

    override def visit[A](fParam: SignatureParameters[TResult] => A, fResult: SignatureResult[TResult] => A): A = fParam(this)
  }

  final case class SignatureResult[TResult[TContext2 <: Context with Singleton, _ <: TypeSystem[TContext2] with Singleton] : SignatureResultConverter]
  (
    result: TResult[context.type, typeSystem.type]
  ) extends Signature[TResult] {

    override def unsubstitutedParameters: Vector[Parameter] = Vector.empty

    override def unsubstitutedResult: TResult[context.type, typeSystem.type] = result

    override def convertTypeSystem[F[_]: Monad](newContext: SignatureContext[context.type])(converter: TypeSystemConverter[context.type, typeSystem.type, newContext.typeSystem.type, F]): F[newContext.Signature[TResult]] =
      for {
        newResult <- implicitly[SignatureResultConverter[TResult]].convertTypeSystem(context)(typeSystem)(newContext.typeSystem)(converter)(result)
      } yield newContext.SignatureResult(newResult)


    override def mapResult[TNewResult[TContext2 <: Context with Singleton, _ <: TypeSystem[TContext2]]](f: TResult[context.type, typeSystem.type] => TNewResult[context.type, typeSystem.type]): Signature[TNewResult] = ???

    override def visit[A](fParam: SignatureParameters[TResult] => A, fResult: SignatureResult[TResult] => A): A = fResult(this)
  }


}
