package dev.argon.compiler.expr

import dev.argon.compiler.*
import dev.argon.compiler.definitions.*
import dev.argon.compiler.signature.*
import dev.argon.compiler.module.{ModuleElementC, ModulePath}
import dev.argon.compiler.tube.{TubeImporter, TubeName}
import dev.argon.parser.IdentifierExpr
import dev.argon.util.{*, given}
import zio.*

import scala.reflect.TypeTest

trait ExprUtilImports extends ExprUtilBase {
  import exprContext.{ArExpr, ExprConstructor, Variable, WrapExpr}

  val tubeImporter: TubeImporter & HasContext[context.type]

  def loadKnownExport[TElement <: ModuleElement[?]]
  (specifier: ImportSpecifier)
  (using TypeTest[ModuleElement[?], TElement])
  : Comp[TElement] =
    def matchesOverload(moduleElement: ModuleElement[?]): Comp[Boolean] =
      val sigEraser = new SignatureEraser {
        override val context: ExprUtilImports.this.context.type = ExprUtilImports.this.context
      }

      def getElementSig(moduleElement: ModuleElement[?]): Comp[ErasedSignature] =
        moduleElement match {
          case ModuleElementC.ClassElement(c) => c.signature.flatMap(sigEraser.erasedNoResult)
          case ModuleElementC.TraitElement(t) => t.signature.flatMap(sigEraser.erasedNoResult)
          case ModuleElementC.FunctionElement(f) => f.signature.flatMap(sigEraser.erasedWithResult)
          case ModuleElementC.ExportedElement(_, _, e) => getElementSig(e)
        }

      getElementSig(moduleElement)
        .map { erasedSig =>
          erasedSig == specifier.signature
        }
    end matchesOverload

    def getElementResult[T](elements: Seq[T]): Comp[T] =
      elements match {
        case Seq(element) => ZIO.succeed(element)
        case elements => ZIO.logDebug(elements.toString) *> ZIO.succeed(???)
      }

    for
      tube <- tubeImporter.getTube(specifier.tube)
      module <- tube.module(specifier.module)
      elements <- module.exports(Set.empty)(specifier.name)
      elements <- ZIO.filter(elements.collect { case element: TElement => element })(matchesOverload)
      result <- getElementResult(elements)
    yield result
  end loadKnownExport

  protected val argonCoreTubeName: TubeName = TubeName(NonEmptyList("Argon", "Core"))


  def boolSpecifier: ImportSpecifier = ImportSpecifier(
    argonCoreTubeName,
    ModulePath(Seq("Bool")),
    Some(IdentifierExpr.Named("Bool")),
    ErasedSignatureNoResult(Seq()),
  )

  def boolType: Comp[WrapExpr] =
    loadKnownExport[ModuleElementC.ClassElement[context.type, ?]](boolSpecifier)
      .map { moduleElement => WrapExpr.OfExpr(ArExpr(ExprConstructor.ClassType(moduleElement.arClass), Vector())) }

  def intSpecifier: ImportSpecifier = ImportSpecifier(
    argonCoreTubeName,
    ModulePath(Seq("Int")),
    Some(IdentifierExpr.Named("Int")),
    ErasedSignatureNoResult(Seq()),
  )

  def intType: Comp[WrapExpr] =
    loadKnownExport[ModuleElementC.ClassElement[context.type, ?]](intSpecifier)
      .map { moduleElement => WrapExpr.OfExpr(ArExpr(ExprConstructor.ClassType(moduleElement.arClass), Vector())) }

  def natSpecifier: ImportSpecifier = ImportSpecifier(
    argonCoreTubeName,
    ModulePath(Seq("Nat")),
    Some(IdentifierExpr.Named("Nat")),
    ErasedSignatureNoResult(Seq()),
  )

  def natType: Comp[WrapExpr] =
    loadKnownExport[ModuleElementC.ClassElement[context.type, ?]](natSpecifier)
      .map { moduleElement => WrapExpr.OfExpr(ArExpr(ExprConstructor.ClassType(moduleElement.arClass), Vector())) }

  def stringSpecifier: ImportSpecifier = ImportSpecifier(
    argonCoreTubeName,
    ModulePath(Seq("String")),
    Some(IdentifierExpr.Named("String")),
    ErasedSignatureNoResult(Seq()),
  )

  def stringType: Comp[WrapExpr] =
    loadKnownExport[ModuleElementC.ClassElement[context.type, ?]](stringSpecifier)
      .map { moduleElement => WrapExpr.OfExpr(ArExpr(ExprConstructor.ClassType(moduleElement.arClass), Vector())) }

  def exceptionSpecifier: ImportSpecifier = ImportSpecifier(
    argonCoreTubeName,
    ModulePath(Seq("Exception")),
    Some(IdentifierExpr.Named("Exception")),
    ErasedSignatureNoResult(Seq()),
  )

  def exceptionType: Comp[WrapExpr] =
    loadKnownExport[ModuleElementC.ClassElement[context.type, ?]](exceptionSpecifier)
      .map { moduleElement => WrapExpr.OfExpr(ArExpr(ExprConstructor.ClassType(moduleElement.arClass), Vector())) }

  def anyType: WrapExpr = WrapExpr.OfExpr(ArExpr(ExprConstructor.AnyType, EmptyTuple))

  def neverType: WrapExpr = WrapExpr.OfExpr(ArExpr(ExprConstructor.NeverType, EmptyTuple))

  def unitValue: WrapExpr = WrapExpr.OfExpr(ArExpr(ExprConstructor.LoadTuple, Seq()))

  def unitType: WrapExpr = unitValue

}
