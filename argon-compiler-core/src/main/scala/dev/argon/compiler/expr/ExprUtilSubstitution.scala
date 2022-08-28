package dev.argon.compiler.expr

import dev.argon.compiler.signature.*
import dev.argon.util.{*, given}
import zio.*

trait ExprUtilSubstitution extends ExprUtilBase {
  import exprContext.{ArExpr, ExprConstructor, Variable, WrapExpr, ClassResult, TraitResult, FunctionResult}


  // Returns the possibly modified expression and the stable version of it
  // Declares a variable if needed
  def asStableExpression(expr: WrapExpr): exprContext.context.Comp[(WrapExpr, WrapExpr)] =
    def isStableConstructor(ctor: ExprConstructor): (Boolean, Boolean) =
      ctor match {
        case ExprConstructor.BindVariable(_) => (true, false)
        case ExprConstructor.ClassConstructorCall(classCtor) => (classCtor.purity, true)
        case ExprConstructor.FunctionCall(func) => (func.purity, true)
        case ExprConstructor.FunctionObjectCall => (true, true)
        case ExprConstructor.LoadConstantBool(_) => (true, false)
        case ExprConstructor.LoadConstantInt(_) => (true, false)
        case ExprConstructor.LoadConstantString(_) => (true, false)
        case ExprConstructor.LoadLambda(_) => (true, false)
        case ExprConstructor.LoadTuple => (true, true)
        case ExprConstructor.LoadTupleElement(_) => (true, true)
        case ExprConstructor.LoadVariable(_) => (true, false)
        case ExprConstructor.MethodCall(method) => (method.purity, true)
        case ExprConstructor.TypeN => (true, true)
        case ExprConstructor.OmegaTypeN(_) => (true, false)
        case ExprConstructor.TraitType(_) => (true, true)
        case ExprConstructor.ClassType(_) => (true, true)
        case ExprConstructor.FunctionType => (true, true)
        case ExprConstructor.UnionType => (true, true)
        case ExprConstructor.IntersectionType => (true, true)
        case ExprConstructor.ExistentialType(_) => (true, true)
        case ExprConstructor.ConjunctionType => (true, true)
        case ExprConstructor.DisjunctionType => (true, true)
        case ExprConstructor.NeverType => (true, true)
        case ExprConstructor.SubtypeWitnessType => (true, true)
        case ExprConstructor.EqualTo => (true, true)
        case ExprConstructor.AssumeErasedValue => (true, true)
        case _ => (false, false)
      }

    def isStableExpression(expr: WrapExpr): Boolean =
      expr match {
        case WrapExpr.OfExpr(expr) =>
          val (isStable, checkArgs) = isStableConstructor(expr.constructor)
          isStable && (!checkArgs || expr.constructor.argsToExprs(expr.args).forall(isStableExpression))

        case WrapExpr.OfHole(_) => true
      }

    if isStableExpression(expr) then
      ZIO.succeed((expr, expr))
    else
      ???
  end asStableExpression

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
  : Comp[Boolean] =
    sig match {
      case Signature.Parameter(_, _, _, paramType, next) =>
        ZIO.succeed(referencesVariable(variable)(paramType)) || referencesVariableSig(variable)(sigHandler)(next)

      case Signature.Result(res) =>
        sigHandler.resultReferences(variable)(res)
    }


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

  private class SubstitutionProcessor(subst: Map[exprContext.Variable, exprContext.WrapExpr]) extends ExprProcessor[Id] :
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
    ZIO.ifZIO(referencesVariableSig(variable)(sigHandler)(sig))(
      onTrue = asStableExpression(replacement).map { case (replacement2, replacementStable) =>
        val sig2 = substituteSignature(variable)(replacement)(sigHandler)(sig)
        (sig2, replacement)
      },
      onFalse = ZIO.succeed((sig, replacement)),
    )

  trait SignatureHandler[Res] {
    def substituteResultMany(subst: Map[Variable, WrapExpr])(res: Res): Res

    def substituteResult(variable: Variable)(replacement: WrapExpr)(res: Res): Res =
      substituteResultMany(Map(variable -> replacement))(res)

    def resultReferences(variable: Variable)(res: Res): Comp[Boolean]
  }

  protected trait FunctionSigHandlerBase extends SignatureHandler[FunctionResult]:
    override def substituteResultMany(subst: Map[Variable, WrapExpr])(res: FunctionResult): FunctionResult =
      FunctionResult(
        returnType = substituteWrapExprMany(subst)(res.returnType),
        ensuresClauses = res.ensuresClauses.map(substituteWrapExprMany(subst))
      )


    override def resultReferences(variable: Variable)(res: FunctionResult): Comp[Boolean] =
      ZIO.succeed(
        referencesVariable(variable)(res.returnType) ||
          res.ensuresClauses.exists(referencesVariable(variable))
      )
  end FunctionSigHandlerBase

  val functionSigHandler: SignatureHandler[FunctionResult] = new FunctionSigHandlerBase {}

  protected trait ClassSigHandlerBase extends SignatureHandler[ClassResult] :
    override def substituteResultMany(subst: Map[Variable, WrapExpr])(res: ClassResult): ClassResult =
      val ClassResult(classType, baseClass, baseTraits) = res

      val classType2 = substituteWrapExprMany(subst)(classType)
      val baseClass2 = baseClass.map { _.map(substituteClassTypeMany(subst)) }
      val baseTraits2 = baseTraits.map { _.map(substituteTraitTypeMany(subst)) }

      ClassResult(classType2, baseClass2, baseTraits2)
    end substituteResultMany

    override def resultReferences(variable: Variable)(res: ClassResult): Comp[Boolean] =
      val ClassResult(classType, baseClass, baseTraits) = res

      ZIO.succeed(referencesVariable(variable)(classType)) ||
        baseClass.map { _.exists { e => referencesVariable(variable)(WrapExpr.OfExpr(e)) } } ||
        baseTraits.map { _.exists { e => referencesVariable(variable)(WrapExpr.OfExpr(e)) } }
    end resultReferences
  end ClassSigHandlerBase

  val classSigHandler: SignatureHandler[ClassResult] = new ClassSigHandlerBase {}

  trait TraitSigHandlerBase extends SignatureHandler[TraitResult]:

    override def substituteResultMany(subst: Map[Variable, WrapExpr])(res: TraitResult): TraitResult =
      val TraitResult(traitType, baseTraits) = res

      val traitType2 = substituteWrapExprMany(subst)(traitType)
      val baseTraits2 = baseTraits.map { _.map(substituteTraitTypeMany(subst)) }

      TraitResult(traitType2, baseTraits2)
    end substituteResultMany

    override def resultReferences(variable: Variable)(res: TraitResult): Comp[Boolean] =
      val TraitResult(traitType, baseTraits) = res

      ZIO.succeed(referencesVariable(variable)(traitType)) ||
        baseTraits.map { _.exists { e => referencesVariable(variable)(WrapExpr.OfExpr(e)) } }
    end resultReferences

  end TraitSigHandlerBase

  val traitSigHandler: SignatureHandler[TraitResult] = new TraitSigHandlerBase {}

  protected trait ClassConstructorSigHandlerBase extends SignatureHandler[Unit] :
    override def substituteResultMany(subst: Map[Variable, WrapExpr])(res: Unit): Unit =
      ()

    override def resultReferences(variable: Variable)(res: Unit): Comp[Boolean] = ZIO.succeed(false)
  end ClassConstructorSigHandlerBase

  val classConstructorSigHandler: SignatureHandler[Unit] = new ClassConstructorSigHandlerBase {}

}
