package dev.argon.plugins.source

import dev.argon.compiler.*
import dev.argon.util.{*, given}
import dev.argon.parser
import dev.argon.parser.IdentifierExpr
import dev.argon.compiler.signature.Signature
import zio.*

object SignatureUtil {

  def create[Res]
  (context: Context)
  (exprConverter: ExpressionConverter with HasContext[context.type])
  (owner: exprConverter.exprContext.ParameterVariableOwner)
  (env: exprConverter.Env)
  (parameters: Seq[WithSource[parser.FunctionParameterList]])
  (createResult: exprConverter.Env => context.Comp[(Res, exprConverter.Env)])
  : context.Comp[(Signature[exprConverter.exprContext.WrapExpr, Res], exprConverter.Env)] =
    import exprConverter.Env
    import exprConverter.exprContext.{WrapExpr, ArExpr, ExprConstructor}

    def impl
    (env: exprConverter.Env)
    (parameters: Seq[WithSource[parser.FunctionParameterList]])
    (index: Int)
    : context.Comp[(Signature[exprConverter.exprContext.WrapExpr, Res], exprConverter.Env)] =
      parameters match
        case WithSource(param, location) +: tail =>
          def convertParamElementTypes(parameters: Seq[WithSource[parser.FunctionParameter]], env: exprConverter.Env, acc: Seq[(IdentifierExpr, WrapExpr)]): context.Comp[(Seq[(IdentifierExpr, WrapExpr)], exprConverter.Env)] =
            parameters match
              case WithSource(parser.FunctionParameter(paramType, name), _) +: tail =>
                exprConverter.convertExpr(paramType).check(env, exprConverter.anyType).flatMap { exprResult =>
                  convertParamElementTypes(tail, exprResult.env, acc :+ (name -> exprResult.expr))
                }

              case _ =>
                ZIO.succeed((acc, env))

            end match

          val typeExpr = WithSource(parser.TupleExpr(param.parameters.map { case WithSource(parser.FunctionParameter(paramType, _), _) => paramType }), location)
          for
            paramElementTypesPair <- convertParamElementTypes(param.parameters, env, Seq())
            (paramElementTypes, env) = paramElementTypesPair

            paramType = WrapExpr.OfExpr(ArExpr(ExprConstructor.LoadTuple, paramElementTypes.map { _._2 }.toVector))
            paramVar = exprConverter.exprContext.ParameterVariable(owner, index, paramType, param.isErased)
            paramVarElements = paramElementTypes
              .zipWithIndex
              .map {
                case ((name, t), i) =>
                  name -> exprConverter.ParameterVariableElement(paramVar, i, t)
              }
              .toMap

            nextPair <- impl(env.withScope(_.addVariable(paramVar).addParameterVariableElements(paramVarElements)))(tail)(index + 1)
            (next, env) = nextPair
          yield (Signature.Parameter(param.listType, param.isErased, paramType, next), env)

        case _ =>
          createResult(env).map {
            case (res, env) =>
              (Signature.Result(res), env)
          }

    impl(env)(parameters)(0)
  end create

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

  def createClassResult
  (context: Context)
  (exprConverter: ExpressionConverter with HasContext[context.type])
  (stmt: parser.ClassDeclarationStmt)
  (env: exprConverter.Env)
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

  def createFunctionResult
  (context: Context)
  (exprConverter: ExpressionConverter with HasContext[context.type])
  (returnTypeExpr: WithSource[parser.Expr])
  (env: exprConverter.Env)
  : context.Comp[(
    exprConverter.exprContext.WrapExpr,
    exprConverter.Env,
  )] =
    exprConverter.convertExpr(returnTypeExpr).check(env, exprConverter.anyType)
      .map { exprResult => (exprResult.expr, exprResult.env) }

  def resolveHolesSig[Res1, Res2](context: Context)(exprConverter: ExpressionConverter with HasContext[context.type])
    (env: exprConverter.Env)(sigHandler: exprConverter.SignatureHandlerPlus[Res1, Res2])
    (sig: Signature[exprConverter.exprContext.WrapExpr, Res2])
    : context.Comp[(Signature[context.ExprContext.WrapExpr, Res1], exprConverter.Env)] =
    sig match {
      case Signature.Parameter(listType, paramErased, paramType, next) =>
        for {
          convParamTypeRes <- exprConverter.resolveHoles(env, paramType)
          (convParamType, env) = convParamTypeRes
          convNextRes <- resolveHolesSig(context)(exprConverter)(env)(sigHandler)(next)
          (convNext, env) = convNextRes
        } yield (Signature.Parameter(listType, paramErased, convParamType, convNext), env)

      case Signature.Result(res) =>
        sigHandler.resolveResultHoles(env, res).map { case (res, env) => (Signature.Result(res), env) }
    }

}
