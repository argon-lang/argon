package dev.argon.backend.js

import cats.data.NonEmptyList
import dev.argon.compiler.core.PayloadSpecifiers.ReferencePayloadSpecifier
import dev.argon.compiler.core._
import dev.argon.compiler.loaders.{ModuleLoader, ResourceIndicator, ResourceReader}
import dev.argon.compiler._
import dev.argon.compiler.expr._
import dev.argon.util.ValueCache
import shapeless.Id
import zio._
import zio.interop.catz.core._
import cats.implicits._

abstract class JSContext private[js] extends ContextWithModule {

  override type TTraitMetadata = Unit
  override type TClassMetadata = Unit
  override type TFunctionMetadata = Unit
  override type TMethodMetadata = Unit
  override type TDataConstructorMetadata = Unit
  override type TClassConstructorMetadata = Unit

  override type TFunctionImplementation = JSImpl.Function
  override type TMethodImplementation = JSImpl.Method
  override type TClassConstructorImplementation = JSImpl.ClassConstructor
  override type TDataConstructorImplementation = JSImpl.DataConstructor

  override type BackendOptions = JSBackendOptions[Id, ResIndicator]

  override def createExprFunctionImplementation(expr: typeSystem.SimpleExpr): JSImpl.Function =
    JSImpl.Function.ExpressionBody(expr)

  override def createExprMethodImplementation(expr: typeSystem.SimpleExpr): JSImpl.Method =
    JSImpl.Method.ExpressionBody(expr)

  override def abstractMethodImplementation: JSImpl.Method = JSImpl.Method.Abstract

  override def createClassConstructorBodyImplementation(body: ClassConstructorBody[this.type]): JSImpl.ClassConstructor =
    JSImpl.ClassConstructor.StatementBody(body)

  override def createDataConstructorImplementation(body: typeSystem.SimpleExpr): JSImpl.DataConstructor =
    JSImpl.DataConstructor.ExpressionBody(body)

  private def getExterns(source: DiagnosticSource): Comp[Map[String, ResolvedExtern]] =
    externFunctionsCache.get(
      compilerInput.backendOptions.extern.files.foldMapM { file =>
        for {
          jsModule <- resourceReader.readTextFileAsString(file)
          map <- extractJSModuleFunctions(jsModule).mapError { _ => DiagnosticError.InvalidExternFunction(source) }
        } yield map.view.mapValues(ResolvedExtern.Function.apply).toMap
      }
    )

  override def createExternFunctionImplementation(specifier: String, source: DiagnosticSource): Comp[JSImpl.Function] =
    getExterns(source).flatMap { externs =>
      externs.get(specifier) match {
        case Some(ResolvedExtern.Function(impl)) => IO.succeed(JSImpl.Function.JSExpressionBody(JSExpressionRaw(impl)))
        case Some(ResolvedExtern.Ambiguous) => Compilation.forErrors(DiagnosticError.AmbiguousExtern(specifier, source))
        case None => Compilation.forErrors(DiagnosticError.UnknownExternImplementation(specifier, source))
      }
    }

  override def createExternMethodImplementation(specifier: String, source: DiagnosticSource): Comp[JSImpl.Method] =
    getExterns(source).flatMap { externs =>
      externs.get(specifier) match {
        case Some(ResolvedExtern.Function(impl)) => IO.succeed(JSImpl.Method.JSExpressionBody(JSExpressionRaw(impl)))
        case Some(ResolvedExtern.Ambiguous) => Compilation.forErrors(DiagnosticError.AmbiguousExtern(specifier, source))
        case None => Compilation.forErrors(DiagnosticError.UnknownExternImplementation(specifier, source))
      }
    }

  object JSImpl {
    import typeSystem._

    sealed trait Function
    object Function {
      final case class JSExpressionBody(expr: JSExpression) extends Function
      final case class ExpressionBody(expr: SimpleExpr) extends Function
    }

    sealed trait Method
    object Method {
      case object Abstract extends Method
      final case class JSExpressionBody(expr: JSExpression) extends Method
      final case class ExpressionBody(expr: SimpleExpr) extends Method
    }

    sealed trait ClassConstructor
    object ClassConstructor {
      final case class StatementBody(body: ClassConstructorBody[JSContext.this.type]) extends ClassConstructor
    }

    sealed trait DataConstructor
    object DataConstructor {
      final case class ExpressionBody(expr: SimpleExpr) extends DataConstructor
    }
  }

  def extractJSModuleFunctions(jsModule: String): IO[Throwable, Map[String, String]]
  protected val externFunctionsCache: ValueCache[CompilationError, Map[String, ResolvedExtern]]
  protected val resourceReader: ResourceReader.Service[ResIndicator]

}
