package dev.argon.compiler.expr

import dev.argon.compiler.signature.*
import dev.argon.expr.{ExprConstraints, ExprTypeBounds, ExprEqualConstraint}
import dev.argon.util.{*, given}
import zio.*

trait ExprUtilHoleResolver
  extends ExprUtilWithHolesBase
    with ExprUtilAccess
    with ExprUtilScope
    with ExprUtilOptions
    with ExprUtilSubstitution
{
  import exprContext.{ArExpr, ExprConstructor, Variable, WrapExpr, ClassResult, TraitResult}

  private class HoleResolver(envRef: Ref.Synchronized[Env]) extends ExprProcessor[Comp] {
    override val context: ExprUtilHoleResolver.this.context.type = ExprUtilHoleResolver.this.context
    override val ec1: exprContext.type = exprContext
    override val ec2: context.ExprContext.type = context.ExprContext

    override def processHole(hole: UniqueIdentifier): Comp[ec2.WrapExpr] =
      for
        resolvedConstraint <- envRef.modify { env =>
          val resolvedConstraint = env.model.get(hole) match {

            case Some(ExprEqualConstraint(other)) => other

            case Some(ExprTypeBounds(_, init :+ last)) =>
              init.foldRight(last) { (a, b) =>
                ec1.WrapExpr.OfExpr(ec1.ArExpr(ec1.ExprConstructor.IntersectionType, (a, b)))
              }

            case Some(ExprTypeBounds(init :+ last, _)) =>
              init.foldRight(last) { (a, b) =>
                ec1.WrapExpr.OfExpr(ec1.ArExpr(ec1.ExprConstructor.UnionType, (a, b)))
              }

            case Some(ExprTypeBounds(_, _)) | None =>
              ec1.WrapExpr.OfExpr(ec1.ArExpr(ec1.ExprConstructor.NeverType, EmptyTuple))
          }

          val env2 = env.copy(model = env.model.updated(hole, ExprEqualConstraint(resolvedConstraint)))

          (resolvedConstraint, env2)
        }

        convResult <- processWrapExpr(resolvedConstraint)
      yield convResult

  }

  def resolveHoles(env: Env, expr: WrapExpr): Comp[(context.ExprContext.WrapExpr, Env)] =
    for
      envRef <- Ref.Synchronized.make(env)
      convExpr <- HoleResolver(envRef).processWrapExpr(expr)
      env <- envRef.get
    yield (convExpr, env)

  def resolveHolesClassType(env: Env, expr: ArExpr[ExprConstructor.ClassType])
  : Comp[(context.ExprContext.ArExpr[context.ExprContext.ExprConstructor.ClassType], Env)] =
    for
      envRef <- Ref.Synchronized.make(env)
      convExpr <- HoleResolver(envRef).processClassType(expr)
      env <- envRef.get
    yield (convExpr, env)

  def resolveHolesTraitType(env: Env, expr: ArExpr[ExprConstructor.TraitType])
  : Comp[(context.ExprContext.ArExpr[context.ExprContext.ExprConstructor.TraitType], Env)] =
    for
      envRef <- Ref.Synchronized.make(env)
      convExpr <- HoleResolver(envRef).processTraitType(expr)
      env <- envRef.get
    yield (convExpr, env)


  def resolveHolesClassConstructorCall(env: Env, expr: ArExpr[ExprConstructor.ClassConstructorCall])
  : Comp[(context.ExprContext.ArExpr[context.ExprContext.ExprConstructor.ClassConstructorCall], Env)] =
    for
      envRef <- Ref.Synchronized.make(env)
      convExpr <- HoleResolver(envRef).processClassConstructorCall(expr)
      env <- envRef.get
    yield (convExpr, env)

  trait SignatureHandlerPlus[Res1, Res2] extends SignatureHandler[Res2] {
    def convertResult(res: Res1): Res2
  }


  override val functionSigHandler: SignatureHandlerPlus[context.ExprContext.WrapExpr, WrapExpr] =
    new SignatureHandlerPlus[context.ExprContext.WrapExpr, WrapExpr] with FunctionSigHandlerBase:
      override def convertResult(res: context.ExprContext.WrapExpr): WrapExpr =
        ExprToHolesConverter(context)(exprContext).processWrapExpr(res)
    end new


  override val classSigHandler: SignatureHandlerPlus[context.ExprContext.ClassResult, ClassResult] =
    new SignatureHandlerPlus[context.ExprContext.ClassResult, ClassResult] with ClassSigHandlerBase:
      override def convertResult(res: context.ExprContext.ClassResult): ClassResult =
        val context.ExprContext.ClassResult(classType, baseClass, baseTraits) = res

        val classType2 =
          ExprToHolesConverter(context)(exprContext).processWrapExpr(classType)
        val baseClass2 =
          baseClass.map { _.map(ExprToHolesConverter(context)(exprContext).processClassType) }
        val baseTraits2 =
          baseTraits.map { _.map(ExprToHolesConverter(context)(exprContext).processTraitType) }

        ClassResult(classType2, baseClass2, baseTraits2)
      end convertResult
    end new


  override val traitSigHandler: SignatureHandlerPlus[context.ExprContext.TraitResult, TraitResult] =
    new SignatureHandlerPlus[context.ExprContext.TraitResult, TraitResult] with TraitSigHandlerBase :
      override def convertResult(res: context.ExprContext.TraitResult): TraitResult =
        val context.ExprContext.TraitResult(traitType, baseTraits) = res

        val traitType2 =
          ExprToHolesConverter(context)(exprContext).processWrapExpr(traitType)
        val baseTraits2 =
          baseTraits.map { _.map(ExprToHolesConverter(context)(exprContext).processTraitType) }

        TraitResult(traitType2, baseTraits2)
      end convertResult
    end new


  override val classConstructorSigHandler: SignatureHandlerPlus[Unit, Unit] =
    new SignatureHandlerPlus[Unit, Unit] with ClassConstructorSigHandlerBase :
      override def convertResult(res: Unit): Unit =
        ()
    end new

  final def convertSig[Res1, Res2](sigHandler: SignatureHandlerPlus[Res1, Res2])
                                  (sig: Signature[context.ExprContext.WrapExpr, Res1])
  : Signature[WrapExpr, Res2] =
    sig match {
      case Signature.Parameter(paramListType, isErased, paramName, paramType, next) =>
        Signature.Parameter(
          paramListType,
          isErased,
          paramName,
          ExprToHolesConverter(context)(exprContext).processWrapExpr(paramType),
          convertSig(sigHandler)(next),
        )

      case Signature.Result(res) =>
        Signature.Result(sigHandler.convertResult(res))
    }

}
