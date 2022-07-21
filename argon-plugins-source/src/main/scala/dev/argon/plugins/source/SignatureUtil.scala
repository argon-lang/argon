package dev.argon.plugins.source

import dev.argon.compiler.*
import dev.argon.util.{*, given}
import dev.argon.parser
import dev.argon.compiler.signature.Signature
import zio.*

object SignatureUtil {

  def create[Res](context: Context)(exprConverter: ExpressionConverter with HasContext[context.type])
    (env: exprConverter.Env)(parameters: Vector[WithSource[parser.FunctionParameterList]])
    (createResult: exprConverter.Env => context.Comp[(Res, exprConverter.Env)])
    : context.Comp[(Signature[exprConverter.exprContext.WrapExpr, Res], exprConverter.Env)] = ???

  def createTraitResult(context: Context)(exprConverter: ExpressionConverter with HasContext[context.type])
    (stmt: parser.TraitDeclarationStmt)(env: exprConverter.Env)
    : context.Comp[(
      (
        exprConverter.exprContext.WrapExpr,
        Seq[exprConverter.exprContext.ArExpr[exprConverter.exprContext.ExprConstructor.TraitType]],
      ),
      exprConverter.Env,
    )] =
    import context.Comp
    import exprConverter.Env
    import exprConverter.exprContext.{WrapExpr, ArExpr, ExprConstructor}

    val zero = WrapExpr.OfExpr(ArExpr(ExprConstructor.LoadConstantInt(0), EmptyTuple))
    val traitType = WrapExpr.OfExpr(ArExpr(ExprConstructor.TypeN, zero))

    def impl(baseType: WrapExpr, baseTraits: Seq[ArExpr[ExprConstructor.TraitType]])
      : Comp[Seq[ArExpr[ExprConstructor.TraitType]]] =
      exprConverter.evaluator.normalizeTopLevelWrap(baseType, exprConverter.fuel).flatMap {
        case WrapExpr.OfExpr(expr) =>
          expr.constructor match {
            case ctor: (expr.constructor.type & ExprConstructor.UnionType.type) =>
              val (a, b) = expr.getArgs(ctor)
              impl(a, baseTraits).flatMap { baseTraits => impl(b, baseTraits) }

            case ctor: (expr.constructor.type & ExprConstructor.TraitType) =>
              ZIO.succeed(baseTraits :+ ArExpr(ctor, expr.getArgs(ctor)))

            case _ => ???
          }

        case WrapExpr.OfHole(_) => ???
      }

    val baseTypes: Comp[(Seq[ArExpr[ExprConstructor.TraitType]], Env)] =
      stmt.baseType match {
        case Some(baseType) =>
          for {
            baseTypeResult <- exprConverter.convertExpr(baseType).check(env, traitType)
            baseTraits <- impl(baseTypeResult.expr, Seq.empty)
          } yield (baseTraits, baseTypeResult.env)

        case None =>
          ZIO.succeed((Seq.empty, env))
      }

    baseTypes.map { case (baseTraits, env) =>
      ((traitType, baseTraits), env)
    }
  end createTraitResult

  def createClassResult(context: Context)(exprConverter: ExpressionConverter with HasContext[context.type])
    (stmt: parser.ClassDeclarationStmt)(env: exprConverter.Env)
    : context.Comp[(
      (
        exprConverter.exprContext.WrapExpr,
        Option[exprConverter.exprContext.ArExpr[exprConverter.exprContext.ExprConstructor.ClassType]],
        Seq[exprConverter.exprContext.ArExpr[exprConverter.exprContext.ExprConstructor.TraitType]],
      ),
      exprConverter.Env,
    )] =
    import context.Comp
    import exprConverter.Env
    import exprConverter.exprContext.{WrapExpr, ArExpr, ExprConstructor}

    val zero = WrapExpr.OfExpr(ArExpr(ExprConstructor.LoadConstantInt(0), EmptyTuple))
    val classType = WrapExpr.OfExpr(ArExpr(ExprConstructor.TypeN, zero))

    def impl
      (
        baseType: WrapExpr,
        baseClass: Option[ArExpr[ExprConstructor.ClassType]],
        baseTraits: Seq[ArExpr[ExprConstructor.TraitType]],
      )
      : Comp[(Option[ArExpr[ExprConstructor.ClassType]], Seq[ArExpr[ExprConstructor.TraitType]])] =
      exprConverter.evaluator.normalizeTopLevelWrap(baseType, exprConverter.fuel).flatMap {
        case WrapExpr.OfExpr(expr) =>
          expr.constructor match {
            case ctor: (expr.constructor.type & ExprConstructor.UnionType.type) =>
              val (a, b) = expr.getArgs(ctor)
              impl(a, baseClass, baseTraits).flatMap { case (baseClass, baseTraits) => impl(b, baseClass, baseTraits) }

            case ctor: (expr.constructor.type & ExprConstructor.TraitType) =>
              ZIO.succeed((baseClass, baseTraits :+ ArExpr(ctor, expr.getArgs(ctor))))

            case ctor: (expr.constructor.type & ExprConstructor.ClassType) =>
              if baseClass.isDefined then
                ???
              else
                ZIO.succeed((Some(ArExpr(ctor, expr.getArgs(ctor))), baseTraits))

            case _ => ???
          }

        case WrapExpr.OfHole(_) => ???
      }

    val baseTypes: Comp[(Option[ArExpr[ExprConstructor.ClassType]], Seq[ArExpr[ExprConstructor.TraitType]], Env)] =
      stmt.baseType match {
        case Some(baseType) =>
          for {
            baseTypeResult <- exprConverter.convertExpr(baseType).check(env, classType)
            baseTypes <- impl(baseTypeResult.expr, None, Seq.empty)
            (baseClass, baseTraits) = baseTypes
          } yield (baseClass, baseTraits, baseTypeResult.env)

        case None =>
          ZIO.succeed((None, Seq.empty, env))
      }

    baseTypes.map { case (baseClass, baseTraits, env) =>
      ((classType, baseClass, baseTraits), env)
    }
  end createClassResult

  def resolveHolesSig[Res1, Res2](context: Context)(exprConverter: ExpressionConverter with HasContext[context.type])
    (env: exprConverter.Env)(sigHandler: exprConverter.SignatureHandlerPlus[Res1, Res2])
    (sig: Signature[exprConverter.exprContext.WrapExpr, Res2])
    : context.Comp[(Signature[context.ExprContext.WrapExpr, Res1], exprConverter.Env)] =
    sig match {
      case Signature.Parameter(listType, paramType, next) =>
        for {
          convParamTypeRes <- exprConverter.resolveHoles(env, paramType)
          (convParamType, env) = convParamTypeRes
          convNextRes <- resolveHolesSig(context)(exprConverter)(env)(sigHandler)(next)
          (convNext, env) = convNextRes
        } yield (Signature.Parameter(listType, convParamType, convNext), env)

      case Signature.Result(res) =>
        sigHandler.resolveResultHoles(env, res).map { case (res, env) => (Signature.Result(res), env) }
    }

}
