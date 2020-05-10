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
import dev.argon.compiler.expr.ArExpr.TypeArgument
import dev.argon.compiler.expr.{ClassConstructorBody, UniverseExpr, WrapperInstance}
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

  def createExprFunctionImplementation(expr: typeSystem.SimpleExpr): TFunctionImplementation
  def createExprMethodImplementation(expr: typeSystem.SimpleExpr): TMethodImplementation
  def abstractMethodImplementation: TMethodImplementation
  def createClassConstructorBodyImplementation(body: ClassConstructorBody[this.type]): TClassConstructorImplementation
  def createDataConstructorImplementation(body: typeSystem.SimpleExpr): TDataConstructorImplementation

  def createExternFunctionImplementation(specifier: String, source: CompilationMessageSource): Comp[TFunctionImplementation]
  def createExternMethodImplementation(specifier: String, source: CompilationMessageSource): Comp[TMethodImplementation]

  object ContextTypeSystem extends TypeSystem {
    override val context: Context.this.type = Context.this

    override type TTypeWrapper[+A] = A
    override val typeWrapperInstances: WrapperInstance[Id] = implicitly

    override def unwrapType[A](t: A): Option[A] = Some(t)

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
    override lazy val context: Context.this.type = Context.this
    override type TTypeWrapper[+A] = A
    override val typeWrapperInstances: WrapperInstance[Id] = implicitly
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
