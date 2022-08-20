package dev.argon.compiler.expr

import dev.argon.compiler.*
import dev.argon.compiler.definitions.*
import dev.argon.compiler.expr.ArgonExprContext
import dev.argon.compiler.module.{ModuleElementC, ModulePath}
import dev.argon.compiler.signature.*
import dev.argon.compiler.tube.TubeName
import dev.argon.expr.{Evaluator, ExprConstraints, ImplicitResolver}
import dev.argon.parser
import dev.argon.parser.IdentifierExpr
import dev.argon.util.{*, given}
import zio.*
import zio.stream.*

import scala.reflect.TypeTest

trait ExprUtil extends UsingContext {
  val exprContext: ArgonExprContext { val context: ExprUtil.this.context.type }
  import exprContext.{ArExpr, ExprConstructor, Variable, WrapExpr}

  def referencesVariable(variable: Variable)(expr: WrapExpr): Boolean =
    expr match {
      case WrapExpr.OfExpr(e) =>
        val hasVariableTop =
          e.constructor match {
            case ExprConstructor.LoadVariable(v) => variable == v
            case ExprConstructor.StoreVariable(v) => variable == v
            case _ => false
          }

        if hasVariableTop then
          true
        else
          e.constructor.argsToExprs(e.args).exists(referencesVariable(variable))
        end if

      case WrapExpr.OfHole(_) => false
    }

  def referencesVariableSig[Res](variable: Variable)(sigHandler: SignatureHandler[Res])(sig: Signature[WrapExpr, Res])
    : Boolean =
    sig match {
      case Signature.Parameter(_, _, _, paramType, next) =>
        referencesVariable(variable)(paramType) || referencesVariableSig(variable)(sigHandler)(next)

      case Signature.Result(res) =>
        sigHandler.resultReferences(variable)(res)
    }

  // Returns the possibly modified expression and the stable version of it
  // Declares a variable if needed
  def asStableExpression(expr: WrapExpr): exprContext.context.Comp[(WrapExpr, WrapExpr)] = ???

  def substituteWrapExprMany(subst: Map[Variable, WrapExpr])(expr: WrapExpr): WrapExpr =
    if subst.isEmpty then
      expr
    else
      SubstitutionProcessor(subst).processWrapExpr(expr)

  def substituteClassTypeMany(subst: Map[Variable, WrapExpr])(expr: ArExpr[ExprConstructor.ClassType]): ArExpr[ExprConstructor.ClassType] =
    if subst.isEmpty then
      expr
    else
      SubstitutionProcessor(subst).processClassType(expr)

  def substituteTraitTypeMany(subst: Map[Variable, WrapExpr])(expr: ArExpr[ExprConstructor.TraitType]): ArExpr[ExprConstructor.TraitType] =
    if subst.isEmpty then
      expr
    else
      SubstitutionProcessor(subst).processTraitType(expr)

  def substituteSignatureMany[Res](subst: Map[Variable, WrapExpr])(sigHandler: SignatureHandler[Res])
                              (sig: Signature[WrapExpr, Res])
  : Signature[WrapExpr, Res] =
    sig match {
      case Signature.Parameter(listType, isErased, paramName, paramType, next) =>
        Signature.Parameter(
          listType,
          isErased,
          paramName,
          substituteWrapExprMany(subst)(paramType),
          substituteSignatureMany(subst)(sigHandler)(next),
        )

      case Signature.Result(res) =>
        Signature.Result(sigHandler.substituteResultMany(subst)(res))
    }

  def substituteSignature[Res](variable: Variable)(replacement: WrapExpr)(sigHandler: SignatureHandler[Res])
                              (sig: Signature[WrapExpr, Res])
  : Signature[WrapExpr, Res] =
    substituteSignatureMany(Map(variable -> replacement))(sigHandler)(sig)

  def substituteMethodCallOwnerType
  (subst: Map[Variable, WrapExpr])
  (ownerType: ExprConstructor.MethodCallOwnerType)
  : ExprConstructor.MethodCallOwnerType =
    ownerType match {
      case ExprConstructor.MethodCallOwnerType.OwnedByTrait(traitType) =>
        ExprConstructor.MethodCallOwnerType.OwnedByTrait(substituteTraitTypeMany(subst)(traitType))

      case ExprConstructor.MethodCallOwnerType.OwnedByClass(classType) =>
        ExprConstructor.MethodCallOwnerType.OwnedByClass(substituteClassTypeMany(subst)(classType))
    }

  private class SubstitutionProcessor(subst: Map[exprContext.Variable, exprContext.WrapExpr]) extends ExprProcessor[Id]:
    override val context: exprContext.context.type = exprContext.context
    override val ec1: exprContext.type = exprContext
    override val ec2: exprContext.type = exprContext

    override def processHole(hole: exprContext.THole): WrapExpr =
      WrapExpr.OfHole(hole)

    override def processArExpr(e: ArExpr[ExprConstructor]): WrapExpr =
      e.constructor match {
        case ctor: (e.constructor.type & ExprConstructor.LoadVariable) =>
          subst.get(ctor.variable) match {
            case Some(replacement) => replacement
            case None => super.processArExpr(e)
          }

        case _ => super.processArExpr(e)
      }
  end SubstitutionProcessor


  def substituedSigResult[Res]
  (owner: exprContext.ParameterVariableOwner)
  (sigHandler: SignatureHandler[Res])
  (sig: Signature[WrapExpr, Res], args: List[WrapExpr]) =
    import exprContext.ParameterVariable

    def impl(paramIndex: Int, sig: Signature[WrapExpr, Res], args: List[WrapExpr]): Comp[Res] =
      (args, sig) match {
        case (arg :: tailArgs, Signature.Parameter(_, paramErased, paramName, paramType, next)) =>
          val variable = ParameterVariable(owner, paramIndex, paramType, paramErased, paramName)
          substituteArg(variable)(arg)(sigHandler)(next).flatMap { case (next, _) =>
            impl(paramIndex + 1, next, tailArgs)
          }

        case (Nil, Signature.Parameter(_, _, _, _, _)) => ???

        case (Nil, Signature.Result(res)) => ZIO.succeed(res)

        case (_ :: _, Signature.Result(_)) => ???
      }

    impl(0, sig, args)
  end substituedSigResult


  // Returns substituted signature and (possibly) modified replacement expression
  def substituteArg[Res]
  (variable: Variable)
  (replacement: WrapExpr)
  (sigHandler: SignatureHandler[Res])
  (sig: Signature[WrapExpr, Res])
  : Comp[(Signature[WrapExpr, Res], WrapExpr)] =
    if referencesVariableSig(variable)(sigHandler)(sig) then
      asStableExpression(replacement).map { case (replacement2, replacementStable) =>
        val sig2 = substituteSignature(variable)(replacement)(sigHandler)(sig)
        (sig2, replacement)
      }
    else
      ZIO.succeed((sig, replacement))

  trait SignatureHandler[Res] {
    def substituteResultMany(subst: Map[Variable, WrapExpr])(res: Res): Res
    def substituteResult(variable: Variable)(replacement: WrapExpr)(res: Res): Res =
      substituteResultMany(Map(variable -> replacement))(res)

    def resultReferences(variable: Variable)(res: Res): Boolean
  }

  final case class Env
  (
    scope: Scope,
    model: Map[exprContext.THole, ExprConstraints[WrapExpr]],
  ) {
    def withScope(f: Scope => Scope): Env = copy(scope = f(scope))

    def mergeBranches(first: Env, second: Env): Env = this

    def allowErased(allow: Boolean): Env = this
  }

  type ClassResultConv = (WrapExpr, Option[ArExpr[ExprConstructor.ClassType]], Seq[ArExpr[ExprConstructor.TraitType]])
  type TraitResultConv = (WrapExpr, Seq[ArExpr[ExprConstructor.TraitType]])

  protected trait FunctionSigHandlerBase extends SignatureHandler[WrapExpr]:
    override def substituteResultMany(subst: Map[Variable, WrapExpr])(res: WrapExpr): WrapExpr =
      substituteWrapExprMany(subst)(res)

    override def resultReferences(variable: Variable)(res: WrapExpr): Boolean = referencesVariable(variable)(res)
  end FunctionSigHandlerBase

  val functionSigHandler: SignatureHandler[WrapExpr] = new FunctionSigHandlerBase {}

  protected trait ClassSigHandlerBase extends SignatureHandler[ClassResultConv]:
    override def substituteResultMany(subst: Map[Variable, WrapExpr])(res: ClassResultConv): ClassResultConv =
      val (classType, baseClass, baseTraits) = res

      val classType2 = substituteWrapExprMany(subst)(classType)
      val baseClass2 = baseClass.map(substituteClassTypeMany(subst))
      val baseTraits2 = baseTraits.map(substituteTraitTypeMany(subst))

      (classType2, baseClass2, baseTraits2)
    end substituteResultMany

    override def resultReferences(variable: Variable)(res: ClassResultConv): Boolean =
      val (classType, baseClass, baseTraits) = res

      referencesVariable(variable)(classType) ||
        baseClass.exists { e => referencesVariable(variable)(WrapExpr.OfExpr(e)) } ||
        baseTraits.exists { e => referencesVariable(variable)(WrapExpr.OfExpr(e)) }
    end resultReferences
  end ClassSigHandlerBase

  val classSigHandler: SignatureHandler[ClassResultConv] = new ClassSigHandlerBase {}

  trait TraitSigHandlerBase extends SignatureHandler[TraitResultConv]:

    override def substituteResultMany(subst: Map[Variable, WrapExpr])(res: TraitResultConv): TraitResultConv =
      val (traitType, baseTraits) = res

      val traitType2 = substituteWrapExprMany(subst)(traitType)
      val baseTraits2 = baseTraits.map(substituteTraitTypeMany(subst))

      (traitType2, baseTraits2)
    end substituteResultMany

    override def resultReferences(variable: Variable)(res: TraitResultConv): Boolean =
      val (traitType, baseTraits) = res

      referencesVariable(variable)(traitType) ||
        baseTraits.exists { e => referencesVariable(variable)(WrapExpr.OfExpr(e)) }
    end resultReferences

  end TraitSigHandlerBase

  val traitSigHandler: SignatureHandler[TraitResultConv] = new TraitSigHandlerBase {}

  protected trait ClassConstructorSigHandlerBase extends SignatureHandler[Unit] :
    override def substituteResultMany(subst: Map[Variable, WrapExpr])(res: Unit): Unit =
      ()

    override def resultReferences(variable: Variable)(res: Unit): Boolean = false
  end ClassConstructorSigHandlerBase

  val classConstructorSigHandler: SignatureHandler[Unit] = new ClassConstructorSigHandlerBase {}

  trait Scope {
    def lookup(id: IdentifierExpr): LookupResult[ScopeElement]

    final def addVariable(variable: Variable): Scope =
      variable.name match
        case Some(id) => Scope.WithValue(id, variable, this)
        case None => this
      end match

    final def addVariables(variables: Seq[Variable]): Scope =
      variables.foldLeft(this)(_.addVariable(_))

    def addParameterVariableElements(map: Map[IdentifierExpr, ParameterVariableElement]): Scope =
      Scope.WithMap(map, this)


  }

  object Scope {

    private final class WithValue(id: IdentifierExpr, value: ScopeElement, next: Scope) extends Scope {
      override def lookup(id: IdentifierExpr): LookupResult[ScopeElement] =
        if this.id == id then
          LookupResult.Success(Seq(value), LookupResult.NotFound())
        else
          next.lookup(id)
    }

    private final class WithMap(map: Map[IdentifierExpr, ScopeElement], next: Scope) extends Scope {
      override def lookup(id: IdentifierExpr): LookupResult[ScopeElement] =
        map.get(id) match
          case Some(value) =>
            LookupResult.Success(Seq(value), LookupResult.NotFound())
          case None =>
            next.lookup(id)
        end match
    }

    def empty: Scope = new Scope {
      override def lookup(id: IdentifierExpr): LookupResult[ScopeElement] = LookupResult.NotFound()
    }

    def fromImports(importsComp: Comp[Imports[context.type]], next: Scope): Scope =
      new Scope {

        override def lookup(id: IdentifierExpr): LookupResult[ScopeElement] =
          LookupResult.Suspended(
            importsComp.map { imports =>
              imports.get(id) match {
                case None | Some(Seq()) => next.lookup(id)
                case Some(elements) => LookupResult.Success(elements, LookupResult.NotFound())
              }
            }
          )

      }

  }

  enum LookupResult[+TElement] {
    case Success(overloads: Seq[TElement], nextPriority: LookupResult[TElement])
    case Suspended(suspendedResult: Comp[LookupResult[TElement]])
    case NotFound()
  }

  final case class ParameterVariableElement(paramVar: exprContext.ParameterVariable, index: Int, elementType: WrapExpr)

  type ScopeElement = ModuleElementC[context.type, ?] | Variable | ParameterVariableElement

  def loadKnownExport[TElement <: ModuleElement[?]]
  (specifier: ImportSpecifier)
  (using TypeTest[ModuleElement[?], TElement])
  : Comp[TElement] =
    def matchesOverload(moduleElement: ModuleElement[?]): Comp[Boolean] =
      val sigEraser = new SignatureEraser {
        override val context: ExprUtil.this.context.type = ExprUtil.this.context
      }

      def getElementSig(moduleElement: ModuleElement[?]): Comp[ErasedSignature] =
        moduleElement match {
          case ModuleElementC.ClassElement(c) => c.signature.flatMap(sigEraser.erasedNoResult)
          case ModuleElementC.TraitElement(t) => t.signature.flatMap(sigEraser.erasedNoResult)
          case ModuleElementC.FunctionElement(f) => f.signature.flatMap(sigEraser.erasedWithResult)
          case ModuleElementC.ExportedElement(e) => getElementSig(e)
        }

      getElementSig(moduleElement)
        .map { erasedSig =>
        erasedSig == specifier.signature
      }
    end matchesOverload

    for
//      stack <- ZIO.stackTrace
//      _ <- ZIO.logTrace(s"loadKnownExport: $specifier\n$stack")

      tube <- context.getTube(specifier.tube)
      module <- tube.module(specifier.module)
      elements <- specifier.name.fold(ZIO.succeed(Seq()))(module.exports(Set.empty))
      elements <- ZIO.filter(elements.collect { case element: TElement => element })(matchesOverload)
      result <- elements match {
        case Seq(element) => ZIO.succeed(element)
        case elements => ZIO.logDebug(elements.toString) *> ZIO.succeed(???)
      }
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
