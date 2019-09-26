package dev.argon.compiler.core

import PayloadSpecifiers._
import dev.argon.compiler._
import dev.argon.compiler.loaders.ModuleLoader
import dev.argon.compiler.loaders.source.SourceModuleCreator
import dev.argon.compiler.lookup._
import dev.argon.compiler.types._
import cats._
import cats.data.NonEmptyList
import cats.evidence.{===, Is}
import cats.implicits._
import shapeless.Nat


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

  type Comp[+A]
  implicit val compCompilationInstance: Compilation[Comp]

  object ContextTypeSystem extends TypeSystem[this.type] {
    override val context: Context.this.type = Context.this
    override val contextProof: Context.this.type === this.context.type = Is.refl

    override type TTypeWrapper[+A] = A

    override type TSComp[A] = context.Comp[A]
    override val tscompCompilationInstance: Compilation[context.Comp] = context.compCompilationInstance

    override def liftComp[A](value: context.Comp[A]): context.Comp[A] = value
    override def liftSignatureResult[TResult[TContext2 <: Context with Singleton, _ <: TypeSystem[TContext2] with Singleton]](sig: context.signatureContext.Signature[TResult, _ <: Nat], args: Vector[TypeArgument]): context.Comp[TResult[Context.this.type, ContextTypeSystem.type]] =
      sig.substituteTypeArguments(sig.unsubstitutedParameters)(args).unsubstitutedResult.pure[context.Comp]

    final override def wrapType[A](a: A): A = a

    override def unwrapType[A](t: A): Option[A] = Some(t)

    final override def mapTypeWrapper[A, B](t: A)(f: A => B): B = f(t)

    final override def flatMapTypeWrapper[A, B](t: A)(f: A => B): B = f(t)

    final override def traverseTypeWrapper[A, B, F[_] : Applicative](t: A)(f: A => F[B]): F[B] =
      f(t)

    override def flatTraverseTypeWrapper[A, B, F[_] : Applicative](t: A)(f: A => F[B]): F[B] =
      f(t)

    override def wrapExprType(expr: WrapExpr): TSComp[TType] =
      getExprType(expr)

    override def isSubTypeWrapper(a: TType, b: TType): TSComp[Option[SubTypeInfo[TType]]] =
      isSimpleSubType(a, b)

    override def universeOfWrapExpr(expr: WrapExpr): TSComp[UniverseExpr] =
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

  val moduleLoaders: Vector[ModuleLoader[this.type]]

  type ResIndicator
  protected val compilerInput: CompilerInput[ResIndicator, BackendOptions]

  def createModule[A](f: ArModule[this.type, DeclarationPayloadSpecifier] => Comp[A])(implicit showRes: Show[ResIndicator], res: ResourceAccess[this.type]): Comp[A] =
    SourceModuleCreator.createModule[A](this)(compilerInput)(f)

}
