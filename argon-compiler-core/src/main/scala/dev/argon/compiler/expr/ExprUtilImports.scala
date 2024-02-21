package dev.argon.compiler.expr

import dev.argon.compiler.*
import dev.argon.compiler.definitions.*
import dev.argon.compiler.signature.*
import dev.argon.compiler.module.{ModuleElementC, ModulePath}
import dev.argon.compiler.tube.{TubeImporter, TubeName}
import dev.argon.expr.ArgonBuiltin
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
  

  def boolType: WrapExpr = WrapExpr.OfExpr(ArExpr[ExprConstructor.Builtin[0]](ExprConstructor.Builtin(ArgonBuiltin.BoolType), NNil))

  def intType: WrapExpr = WrapExpr.OfExpr(ArExpr[ExprConstructor.Builtin[0]](ExprConstructor.Builtin(ArgonBuiltin.IntType), NNil))

  def stringType: WrapExpr = WrapExpr.OfExpr(ArExpr[ExprConstructor.Builtin[0]](ExprConstructor.Builtin(ArgonBuiltin.StringType), NNil))

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

  def neverType: WrapExpr = WrapExpr.OfExpr(ArExpr[ExprConstructor.Builtin[0]](ExprConstructor.Builtin(ArgonBuiltin.NeverType), NNil))

  def unitValue: WrapExpr = WrapExpr.OfExpr(ArExpr(ExprConstructor.LoadTuple, Seq()))

  def unitType: WrapExpr = unitValue

}
