package dev.argon.backend.jvm

import cats.data.NonEmptyList
import dev.argon.compiler._
import dev.argon.compiler.core.ContextWithModule
import dev.argon.compiler.expr.ClassConstructorBody
import dev.argon.compiler.loaders.ResourceReader
import dev.argon.util.{MemoCacheStore, ValueCache}
import org.objectweb.asm.MethodVisitor
import org.objectweb.asm.tree.MethodNode
import shapeless.Id
import zio.{Has, IO, Ref, ZLayer}
import zio.blocking.Blocking
import zio.stream.Stream

abstract class JVMContext private[jvm] extends ContextWithModule {

  override type TFunctionImplementation = JVMImpl.Function
  override type TMethodImplementation = JVMImpl.Method
  override type TClassConstructorImplementation = JVMImpl.ClassConstructor
  override type TDataConstructorImplementation = JVMImpl.DataConstructor

  override type BackendOptions = JVMBackendOptions[Id, ResIndicator]

  override def createExprFunctionImplementation(expr: typeSystem.SimpleExpr): JVMImpl.Function =
    JVMImpl.Function.ExpressionBody(expr)

  override def createExprMethodImplementation(expr: typeSystem.SimpleExpr): JVMImpl.Method =
    JVMImpl.Method.ExpressionBody(expr)

  override def abstractMethodImplementation: JVMImpl.Method = JVMImpl.Method.Abstract

  override def createClassConstructorBodyImplementation(body: ClassConstructorBody[this.type]): JVMImpl.ClassConstructor =
    JVMImpl.ClassConstructor.StatementBody(body)

  override def createDataConstructorImplementation(body: typeSystem.SimpleExpr): JVMImpl.DataConstructor =
    JVMImpl.DataConstructor.ExpressionBody(body)

  private def getExtern(specifier: String, source: DiagnosticSource): Comp[MethodNode] =
    externFunctionsCache.get(
      ExternMethodExtractor.getExterns(
        Stream.fromIterable(compilerInput.backendOptions.extern.files)
      )
        .provide(Has.allOf[ResourceReader.Service[ResIndicator], Blocking.Service](resourceReader, blockingService))
    )
      .mapError { _ => DiagnosticError.InvalidExternFunction(source) }
      .flatMap { externs =>
        externs.get(specifier) match {
          case Some(ResolvedExtern.Method(node)) => IO.succeed(node)
          case Some(ResolvedExtern.Ambiguous) => Compilation.forErrors(DiagnosticError.AmbiguousExtern(specifier, source))
          case None => Compilation.forErrors(DiagnosticError.UnknownExternImplementation(specifier, source))
        }
      }

  private def getActionBodyForExtern(extern: MethodNode): MethodVisitor => Comp[Unit] = visitor =>
    blockingService.effectBlocking {
      extern.accept(visitor)
    }
      .catchAll(Compilation.unwrapThrowable)


  override def createExternFunctionImplementation(specifier: String, source: DiagnosticSource): Comp[JVMImpl.Function] =
    getExtern(specifier, source).map { extern =>
      JVMImpl.Function.MethodVisitorActionBody(getActionBodyForExtern(extern))
    }

  override def createExternMethodImplementation(specifier: String, source: DiagnosticSource): Comp[JVMImpl.Method] =
    getExtern(specifier, source).map { extern =>
      JVMImpl.Method.MethodVisitorActionBody(getActionBodyForExtern(extern))
    }

  private[jvm] object JVMImpl {
    import typeSystem._

    sealed trait Function
    object Function {
      final case class MethodVisitorActionBody(func: MethodVisitor => Comp[Unit]) extends Function
      final case class ExpressionBody(expr: SimpleExpr) extends Function
    }

    sealed trait Method
    object Method {
      case object Abstract extends Method
      final case class MethodVisitorActionBody(func: MethodVisitor => Comp[Unit]) extends Method
      final case class ExpressionBody(expr: SimpleExpr) extends Method
    }

    sealed trait ClassConstructor
    object ClassConstructor {
      final case class StatementBody(body: ClassConstructorBody[JVMContext.this.type]) extends ClassConstructor
    }

    sealed trait DataConstructor
    object DataConstructor {
      final case class ExpressionBody(expr: SimpleExpr) extends DataConstructor
    }


  }

  protected val externFunctionsCache: ValueCache[Throwable, Map[String, ResolvedExtern]]
  protected val resourceReader: ResourceReader.Service[ResIndicator]
  protected val blockingService: Blocking.Service


}
