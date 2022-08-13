package dev.argon.plugins.source

import dev.argon.compiler.*
import dev.argon.compiler.expr.ArgonExprContext
import dev.argon.util.{*, given}
import dev.argon.parser
import dev.argon.parser.IdentifierExpr
import dev.argon.compiler.signature.Signature
import zio.*

object SignatureUtil {

  def create[Res]
  (context: Context)
  (exprConverter: ExpressionConverter & HasContext[context.type])
  (owner: exprConverter.exprContext.ParameterVariableOwner)
  (env: exprConverter.Env)
  (parameters: Seq[WithSource[parser.FunctionParameterList]])
  (createResult: exprConverter.Env => context.Comp[Res])
  : context.Comp[(Signature[context.ExprContext.WrapExpr, Res], exprConverter.Env)] =
    import exprConverter.Env

    def impl
    (env: exprConverter.Env)
    (parameters: Seq[WithSource[parser.FunctionParameterList]])
    (index: Int)
    : context.Comp[(Signature[context.ExprContext.WrapExpr, Res], exprConverter.Env)] =
      parameters match
        case WithSource(param, location) +: tail =>
          def convertParamElementType(param: parser.FunctionParameter, env: exprConverter.Env): context.Comp[context.ExprContext.WrapExpr] =
            for
              exprResult <- exprConverter.convertExpr(param.paramType).check(env, exprConverter.anyType)
              resExprPair <- exprConverter.resolveHoles(exprResult.env, exprResult.expr)
            yield resExprPair._1

          def convertParamElementTypes(parameters: Seq[WithSource[parser.FunctionParameter]], env: exprConverter.Env, acc: Seq[(IdentifierExpr, context.ExprContext.WrapExpr)]): context.Comp[Seq[(IdentifierExpr, context.ExprContext.WrapExpr)]] =
            parameters match
              case WithSource(param, _) +: tail =>
                convertParamElementType(param, env).flatMap { resExpr =>
                  convertParamElementTypes(tail, env, acc :+ (param.name -> resExpr))
                }

              case _ =>
                ZIO.succeed(acc)

            end match

          param match {
            case parser.FunctionParameterList(_, _, Vector(WithSource(paramElem, _)), false) =>
              for
                paramType <- convertParamElementType(paramElem, env)
                paramType2 = ArgonExprContext.convertWrapExpr[Id](context)(context.ExprContext, exprConverter.exprContext)(identity)(paramType)
                paramVar = exprConverter.exprContext.ParameterVariable(owner, index, paramType2, param.isErased, Some(paramElem.name))

                (next, env) <- impl(env.withScope(_.addVariable(paramVar)))(tail)(index + 1)
              yield (Signature.Parameter(param.listType, param.isErased, Some(paramElem.name), paramType, next), env)

            case _ =>
              for
                paramElementTypes <- convertParamElementTypes(param.parameters, env, Seq())

                paramType = context.ExprContext.WrapExpr.OfExpr(
                  context.ExprContext.ArExpr(
                    context.ExprContext.ExprConstructor.LoadTuple,
                    paramElementTypes.map { _._2 }.toVector
                  )
                )
                paramType2 = ArgonExprContext.convertWrapExpr[Id](context)(context.ExprContext, exprConverter.exprContext)(identity)(paramType)
                paramVar = exprConverter.exprContext.ParameterVariable(owner, index, paramType2, param.isErased, None)
                paramVarElements = paramElementTypes
                  .zipWithIndex
                  .map {
                    case ((name, t), i) =>
                      val t2 = ArgonExprContext.convertWrapExpr[Id](context)(context.ExprContext, exprConverter.exprContext)(identity)(t)
                      name -> exprConverter.ParameterVariableElement(paramVar, i, t2)
                  }
                  .toMap

                (next, env) <- impl(env.withScope(_.addVariable(paramVar).addParameterVariableElements(paramVarElements)))(tail)(index + 1)
              yield (Signature.Parameter(param.listType, param.isErased, None, paramType, next), env)
          }

        case _ =>
          createResult(env).map { res =>
              (Signature.Result(res), env)
          }

    impl(env)(parameters)(0)
  end create

  def createTraitResult(context: Context)(exprConverter: ExpressionConverter & HasContext[context.type])
    (stmt: parser.TraitDeclarationStmt)(env: exprConverter.Env)
    : context.Comp[(
      context.ExprContext.WrapExpr,
      Seq[context.ExprContext.ArExpr[context.ExprContext.ExprConstructor.TraitType]],
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

    for
      (baseTraits, env) <- baseTypes

      (traitType2, env) <- exprConverter.resolveHoles(env, traitType)

      envCell <- Ref.make(env)

      baseTraits2 <- ZIO.foreach(baseTraits) { baseTrait =>
        for
          env <- envCell.get
          (res, env) <- exprConverter.resolveHolesTraitType(env, baseTrait)
          _ <- envCell.set(env)
        yield res
      }
    yield (traitType2, baseTraits2)
  end createTraitResult

  def createClassResult
  (context: Context)
  (exprConverter: ExpressionConverter & HasContext[context.type])
  (stmt: parser.ClassDeclarationStmt)
  (env: exprConverter.Env)
  : context.Comp[(
    context.ExprContext.WrapExpr,
    Option[context.ExprContext.ArExpr[context.ExprContext.ExprConstructor.ClassType]],
    Seq[context.ExprContext.ArExpr[context.ExprContext.ExprConstructor.TraitType]],
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
            (baseClass, baseTraits) <- impl(baseTypeResult.expr, None, Seq.empty)
          } yield (baseClass, baseTraits, baseTypeResult.env)

        case None =>
          ZIO.succeed((None, Seq.empty, env))
      }

    for
      (baseClass, baseTraits, env) <- baseTypes

      (classType2, env) <- exprConverter.resolveHoles(env, classType)

      envCell <- Ref.make(env)

      baseClass2 <- ZIO.foreach(baseClass) { baseClass =>
        for
          env <- envCell.get
          (res, env) <- exprConverter.resolveHolesClassType(env, baseClass)
          _ <- envCell.set(env)
        yield res
      }

      baseTraits2 <- ZIO.foreach(baseTraits) { baseTrait =>
        for
          env <- envCell.get
          (res, env) <- exprConverter.resolveHolesTraitType(env, baseTrait)
          _ <- envCell.set(env)
        yield res
      }
    yield (classType2, baseClass2, baseTraits2)
  end createClassResult

  def createFunctionResult
  (context: Context)
  (exprConverter: ExpressionConverter & HasContext[context.type])
  (returnTypeExpr: WithSource[parser.Expr])
  (env: exprConverter.Env)
  : context.Comp[context.ExprContext.WrapExpr] =
    exprConverter.convertExpr(returnTypeExpr).check(env, exprConverter.anyType)
      .flatMap { exprResult => exprConverter.resolveHoles(exprResult.env, exprResult.expr) }
      .map { _._1 }


  def resolveHolesSig[Res1, Res2](context: Context)(exprConverter: ExpressionConverter & HasContext[context.type])
    (env: exprConverter.Env)(sigHandler: exprConverter.SignatureHandlerPlus[Res1, Res2])
    (sig: Signature[exprConverter.exprContext.WrapExpr, Res2])
    : context.Comp[(Signature[context.ExprContext.WrapExpr, Res1], exprConverter.Env)] =
    sig match {
      case Signature.Parameter(listType, paramErased, paramName, paramType, next) =>
        for {
          (convParamType, env) <- exprConverter.resolveHoles(env, paramType)
          (convNext, env) <- resolveHolesSig(context)(exprConverter)(env)(sigHandler)(next)
        } yield (Signature.Parameter(listType, paramErased, paramName, convParamType, convNext), env)

      case Signature.Result(res) =>
        sigHandler.resolveResultHoles(env, res).map { case (res, env) => (Signature.Result(res), env) }
    }

}
