package dev.argon.compiler.core

import PayloadSpecifiers._
import dev.argon.compiler._
import dev.argon.compiler.loaders.{ModuleLoad, ResourceIndicator, ResourceReader, SourceParser}
import dev.argon.compiler.lookup._
import dev.argon.compiler.types._
import cats._
import cats.data.NonEmptyList
import cats.evidence.{===, Is}
import cats.implicits._
import dev.argon.compiler.expr.{ClassConstructorBody, UniverseExpr, WrapperInstance}
import dev.argon.compiler.options.CompilerInput
import shapeless.Nat
import zio._


trait Context {

  type TFunctionImplementation
  type TMethodImplementation
  type TDataConstructorImplementation
  type TClassConstructorImplementation

  type BackendOptions

  def createExprFunctionImplementation(expr: typeSystem.SimpleExpr): TFunctionImplementation
  def createExprMethodImplementation(expr: typeSystem.SimpleExpr): TMethodImplementation
  def abstractMethodImplementation: TMethodImplementation
  def createClassConstructorBodyImplementation(body: ClassConstructorBody[this.type]): TClassConstructorImplementation
  def createDataConstructorImplementation(body: typeSystem.SimpleExpr): TDataConstructorImplementation

  def createExternFunctionImplementation(specifier: String, source: DiagnosticSource): Comp[TFunctionImplementation]
  def createExternMethodImplementation(specifier: String, source: DiagnosticSource): Comp[TMethodImplementation]

  object ContextTypeSystem extends TypeSystem {
    override val context: Context.this.type = Context.this

    override type TTypeWrapper[+A] = A
    override val typeWrapperInstances: WrapperInstance[Id] = implicitly

    override def unwrapType[A](t: A): Option[A] = Some(t)

    override def wrapExprType(expr: WrapExpr): Comp[TType] =
      getExprType(expr)

    override def isSubTypeWrapper(a: TType, b: TType): Comp[Option[SubTypeInfo[TType]]] =
      isSimpleSubType(a, b)

    override def isSubTypeWrapperImpl[A](a: A, b: A): Comp[Either[(A, A), Option[SubTypeInfo[A]]]] =
      IO.succeed(Left((a, b)))

    override def universeOfWrapExpr(expr: WrapExpr): Comp[UniverseExpr] =
      universeOfExpr(expr)

    override def universeOfWrapExprImpl[A](expr: A): Comp[Either[A, UniverseExpr]] =
      IO.succeed(Left(expr))
  }

  final lazy val typeSystem: ContextTypeSystem.type = ContextTypeSystem
  final lazy val scopeContext: ScopeContext[this.type] { val typeSystem: Context.this.typeSystem.type } = new ScopeContext[this.type] {
    override val context: Context.this.type = Context.this
    override lazy val typeSystem: Context.this.typeSystem.type = Context.this.typeSystem
  }

  object ContextSignatureContext extends SignatureContext {
    override lazy val context: Context.this.type = Context.this
    override type TTypeWrapper[+A] = A
    override val typeWrapperInstances: WrapperInstance[Id] = implicitly
  }

  final lazy val signatureContext: ContextSignatureContext.type = ContextSignatureContext

  type ResIndicator <: ResourceIndicator
  implicit val resIndicatorTag: Tag[ResIndicator]
  protected val compilerInput: CompilerInput[ResIndicator, BackendOptions]

  def module[TContext >: this.type <: Context.WithRes[ResIndicator]: Tag]: ZManaged[ModuleLoad[ResIndicator, TContext] with ResourceReader[ResIndicator] with SourceParser, CompilationError, ArModule[this.type, DeclarationPayloadSpecifier]]

}

object Context {
  type WithRes[I <: ResourceIndicator] = Context { type ResIndicator = I }


}
