package dev.argon.compiler.core

import dev.argon.compiler._
import dev.argon.compiler.lookup._
import dev.argon.compiler.types._
import cats._
import dev.argon.backend.{Backend, ExternHandler}
import dev.argon.compiler.expr.{ClassConstructorBody, UniverseExpr, WrapperInstance}
import dev.argon.compiler.options.CompilerInput
import zio._


trait Context {

  val backend: Backend
  val externHandler: backend.TExternHandler

  type TFunctionImplementation = FunctionImplementation[externHandler.ExternFunction, typeSystem.SimpleExpr]
  type TMethodImplementation = MethodImplementation[externHandler.ExternMethod, typeSystem.SimpleExpr]
  type TDataConstructorImplementation = DataConstructorImplementation[typeSystem.SimpleExpr]
  type TClassConstructorImplementation = ClassConstructorImplementation[ClassConstructorBody[this.type]]

  def createExprFunctionImplementation(expr: typeSystem.SimpleExpr): TFunctionImplementation = FunctionImplementation.Expression(expr)
  def createExprMethodImplementation(expr: typeSystem.SimpleExpr): TMethodImplementation = MethodImplementation.Expression(expr)
  def abstractMethodImplementation: TMethodImplementation = MethodImplementation.Abstract
  def createClassConstructorBodyImplementation(body: ClassConstructorBody[this.type]): TClassConstructorImplementation = ClassConstructorImplementation(body)
  def createDataConstructorImplementation(body: typeSystem.SimpleExpr): TDataConstructorImplementation = DataConstructorImplementation(body)

  def createExternFunctionImplementation(specifier: String, source: DiagnosticSource): Comp[TFunctionImplementation] =
    externHandler.loadExternFunction(source, specifier).map(FunctionImplementation.Extern(source, _))

  def createExternMethodImplementation(specifier: String, source: DiagnosticSource): Comp[TMethodImplementation] =
    externHandler.loadExternMethod(source, specifier).map(MethodImplementation.Extern(source, _))

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

}

object Context {
  type Aux[TBackend <: Backend] = Context { val backend: TBackend }

  def make(backend2: Backend)(input: CompilerInput[backend2.BackendOptionID]): UIO[Aux[backend2.type]] = for {
    externs <- backend2.externHandler(input.backendOptions)
  } yield new Context {
    override val backend: backend2.type = backend2
    override val externHandler: backend.TExternHandler = externs
  }
}
