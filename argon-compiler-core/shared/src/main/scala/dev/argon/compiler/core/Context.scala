package dev.argon.compiler.core

import PayloadSpecifiers._
import dev.argon.compiler._
import dev.argon.compiler.loaders.{ModuleLoad, ResourceIndicator}
import dev.argon.compiler.lookup._
import dev.argon.compiler.types._
import cats._
import cats.data.NonEmptyList
import cats.evidence.{===, Is}
import cats.implicits._
import shapeless.Nat
import zio._


trait Context {

  type TFunctionImplementation
  type TMethodImplementation
  type TDataConstructorImplementation
  type TClassConstructorImplementation

  type TFunctionMetadata
  type TMethodMetadata
  type TTraitMetadata
  type TClassMetadata
  type TDataConstructorMetadata
  type TClassConstructorMetadata

  type BackendOptions

  def createExprFunctionImplementation(expr: typeSystem.ArExpr): TFunctionImplementation
  def createExprMethodImplementation(expr: typeSystem.ArExpr): TMethodImplementation
  def abstractMethodImplementation: TMethodImplementation
  def createClassConstructorBodyImplementation(body: typeSystem.ClassConstructorBody): TClassConstructorImplementation
  def createDataConstructorImplementation(body: typeSystem.ArExpr): TDataConstructorImplementation

  def createExternFunctionImplementation(specifier: String, source: CompilationMessageSource): Comp[TFunctionImplementation]
  def createExternMethodImplementation(specifier: String, source: CompilationMessageSource): Comp[TMethodImplementation]

  object ContextTypeSystem extends TypeSystem[this.type] {
    override val context: Context.this.type = Context.this
    override val contextProof: Context.this.type === this.context.type = Is.refl

    override type TTypeWrapper[+A] = A

    override def liftSignatureResult[TResult[TContext2 <: Context with Singleton, _ <: TypeSystem[TContext2] with Singleton]](sig: context.signatureContext.Signature[TResult, _ <: Nat], args: Vector[TypeArgument]): Comp[TResult[Context.this.type, ContextTypeSystem.type]] =
      sig.substituteTypeArguments(sig.unsubstitutedParameters)(args).map { _.unsubstitutedResult }

    final override def wrapType[A](a: A): A = a

    override def unwrapType[A](t: A): Option[A] = Some(t)

    final override def mapTypeWrapper[A, B](t: A)(f: A => B): B = f(t)

    final override def flatMapTypeWrapper[A, B](t: A)(f: A => B): B = f(t)

    final override def traverseTypeWrapper[A, B, F[_] : Applicative](t: A)(f: A => F[B]): F[B] =
      f(t)

    override def flatTraverseTypeWrapper[A, B, F[_] : Applicative](t: A)(f: A => F[B]): F[B] =
      f(t)

    override def wrapExprType(expr: WrapExpr): Comp[TType] =
      getExprType(expr)

    override def isSubTypeWrapper(a: TType, b: TType): Comp[Option[SubTypeInfo[TType]]] =
      isSimpleSubType(a, b)

    override def universeOfWrapExpr(expr: WrapExpr): Comp[UniverseExpr] =
      universeOfExpr(expr)
  }

  final lazy val typeSystem: ContextTypeSystem.type = ContextTypeSystem
  final lazy val scopeContext: ScopeContext[this.type] { val typeSystem: Context.this.typeSystem.type } = new ScopeContext[this.type] {
    override val context: Context.this.type = Context.this
    override lazy val typeSystem: Context.this.typeSystem.type = Context.this.typeSystem
  }

  object ContextSignatureContext extends SignatureContext {
    override val context: Context.this.type = Context.this
    override lazy val typeSystem: Context.this.typeSystem.type = Context.this.typeSystem
  }

  final lazy val signatureContext: ContextSignatureContext.type = ContextSignatureContext

  type ResIndicator <: ResourceIndicator
  implicit val resIndicatorTag: Tagged[ResIndicator]
  protected val compilerInput: CompilerInput[ResIndicator, BackendOptions]

  def module[TContext >: this.type <: Context.WithRes[ResIndicator]: Tagged]: ZManaged[ModuleLoad[ResIndicator, TContext], ErrorList, ArModule[this.type, DeclarationPayloadSpecifier]]

}

object Context {
  type WithRes[I <: ResourceIndicator] = Context { type ResIndicator = I }
}
