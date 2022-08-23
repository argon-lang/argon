package dev.argon.compiler.expr

import dev.argon.compiler.signature.*
import dev.argon.util.{*, given}
import zio.*

trait ExprUtilHoleResolver
  extends ExprUtilWithHolesBase
    with ExprUtilAccess
    with ExprUtilScope
    with ExprUtilOptions
    with ExprUtilSubstitution
{
  import exprContext.{ArExpr, ExprConstructor, Variable, WrapExpr}

  private class HoleResolver(envRef: Ref.Synchronized[Env]) extends ExprProcessor[Comp] {
    override val context: ExprUtilHoleResolver.this.context.type = ExprUtilHoleResolver.this.context
    override val ec1: exprContext.type = exprContext
    override val ec2: context.ExprContext.type = context.ExprContext

    override def processHole(hole: UniqueIdentifier): Comp[ec2.WrapExpr] = ???
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

  type ClassResultContext = (context.ExprContext.WrapExpr, Option[context.ExprContext.ArExpr[context.ExprContext.ExprConstructor.ClassType]], Seq[context.ExprContext.ArExpr[context.ExprContext.ExprConstructor.TraitType]])
  type TraitResultContext = (context.ExprContext.WrapExpr, Seq[context.ExprContext.ArExpr[context.ExprContext.ExprConstructor.TraitType]])


  trait SignatureHandlerPlus[Res1, Res2] extends SignatureHandler[Res2] {
    def convertResult(res: Res1): Res2

    def resolveResultHoles(env: Env, res: Res2): Comp[(Res1, Env)]
  }


  override val functionSigHandler: SignatureHandlerPlus[context.ExprContext.WrapExpr, WrapExpr] =
    new SignatureHandlerPlus[context.ExprContext.WrapExpr, WrapExpr] with FunctionSigHandlerBase :
      override def convertResult(res: context.ExprContext.WrapExpr): WrapExpr =
        ExprToHolesConverter(context)(exprContext).processWrapExpr(res)

      override def resolveResultHoles(env: Env, res: WrapExpr): Comp[(context.ExprContext.WrapExpr, Env)] =
        resolveHoles(env, res)
    end new


  override val classSigHandler: SignatureHandlerPlus[ClassResultContext, ClassResultConv] =
    new SignatureHandlerPlus[ClassResultContext, ClassResultConv] with ClassSigHandlerBase :
      override def convertResult(res: ClassResultContext): ClassResultConv =
        val (classType, baseClass, baseTraits) = res

        val classType2 =
          ExprToHolesConverter(context)(exprContext).processWrapExpr(classType)
        val baseClass2 =
          baseClass.map(ExprToHolesConverter(context)(exprContext).processClassType)
        val baseTraits2 =
          baseTraits.map(ExprToHolesConverter(context)(exprContext).processTraitType)

        (classType2, baseClass2, baseTraits2)
      end convertResult

      override def resolveResultHoles(env: Env, res: ClassResultConv): Comp[(ClassResultContext, Env)] =
        val (classType, baseClass, baseTraits) = res
        resolveHoles(env, classType).flatMap { case (classType2, env) =>
          ZIO.foreach(baseClass)(resolveHolesClassType(env, _))
            .map {
              case Some((baseClass2, env)) => (Some(baseClass2), env)
              case None => (None, env)
            }
            .flatMap { case (baseClass2, env) =>
              for {
                envState <- Ref.make(env)
                baseTraits2 <-
                  ZIO.foreach(baseTraits) { baseTrait =>
                    for {
                      env <- envState.get
                      (baseTrait2, env) <- resolveHolesTraitType(env, baseTrait)
                      _ <- envState.set(env)
                    } yield baseTrait2
                  }
                env <- envState.get
              } yield ((classType2, baseClass2, baseTraits2), env)

            }
        }
      end resolveResultHoles
    end new


  override val traitSigHandler: SignatureHandlerPlus[TraitResultContext, TraitResultConv] =
    new SignatureHandlerPlus[TraitResultContext, TraitResultConv] with TraitSigHandlerBase :
      override def convertResult(res: TraitResultContext): TraitResultConv =
        val (traitType, baseTraits) = res

        val traitType2 =
          ExprToHolesConverter(context)(exprContext).processWrapExpr(traitType)
        val baseTraits2 =
          baseTraits.map(ExprToHolesConverter(context)(exprContext).processTraitType)

        (traitType2, baseTraits2)
      end convertResult

      override def resolveResultHoles(env: Env, res: TraitResultConv): Comp[(TraitResultContext, Env)] =
        val (classType, baseTraits) = res
        resolveHoles(env, classType).flatMap { case (classType2, env) =>
          for {
            envState <- Ref.make(env)
            baseTraits2 <-
              ZIO.foreach(baseTraits) { baseTrait =>
                for {
                  env <- envState.get
                  (baseTrait2, env) <- resolveHolesTraitType(env, baseTrait)
                  _ <- envState.set(env)
                } yield baseTrait2
              }
            env <- envState.get
          } yield ((classType2, baseTraits2), env)
        }
      end resolveResultHoles
    end new


  override val classConstructorSigHandler: SignatureHandlerPlus[Unit, Unit] =
    new SignatureHandlerPlus[Unit, Unit] with ClassConstructorSigHandlerBase :
      override def convertResult(res: Unit): Unit =
        ()

      override def resolveResultHoles(env: Env, res: Unit): Comp[(Unit, Env)] =
        ZIO.succeed(((), env))
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
