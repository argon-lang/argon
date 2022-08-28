package dev.argon.plugins.source

import dev.argon.compiler.*
import dev.argon.compiler.expr.{ArgonExprContext, ExprToHolesConverter}
import dev.argon.util.{*, given}
import dev.argon.parser
import dev.argon.parser.{FunctionParameterListType, IdentifierExpr}
import dev.argon.compiler.signature.Signature
import zio.*

object SignatureUtil {

  def create[Res]
  (context: Context)
  (exprConverter: ExpressionConverter & HasContext[context.type])
  (owner: exprConverter.exprContext.ParameterVariableOwner)
  (env: exprConverter.Env)
  (parameters: Seq[WithSource[parser.FunctionParameterList]])
  (createResult: (exprConverter.Env, exprConverter.ExprOptions) => context.Comp[Res])
  : context.Comp[(Signature[context.ExprContext.WrapExpr, Res], exprConverter.Env)] =
    import exprConverter.Env


    val opt = exprConverter.ExprOptions(
      purity = true,
      accessToken = createAccessToken(exprConverter)(owner),
      allowAbstractConstructorCall = false,
      allowErased = false,
      postconditions = None,
    )

    def impl
    (env: exprConverter.Env)
    (parameters: Seq[WithSource[parser.FunctionParameterList]])
    (index: Int)
    : context.Comp[(Signature[context.ExprContext.WrapExpr, Res], exprConverter.Env)] =
      parameters match
        case WithSource(param, location) +: tail =>
          def convertParamElementType(param: parser.FunctionParameter, env: exprConverter.Env): context.Comp[context.ExprContext.WrapExpr] =
            for
              exprResult <- exprConverter.convertExpr(param.paramType).check(env, opt.forTypeExpr, exprConverter.anyType)
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

          def addVariableToEnv(env: exprConverter.Env, listType: FunctionParameterListType, paramVar: exprConverter.exprContext.ParameterVariable): exprConverter.Env =
            val env2 = env.withScope(_.addVariable(paramVar))
            val env3 =
              if listType == FunctionParameterListType.RequiresList then
                env2.withImplicitSource(_.addVariable(paramVar))
              else
                env2

            env3
          end addVariableToEnv


          param match {
            case parser.FunctionParameterList(listType, _, Vector(WithSource(paramElem, _)), false) =>
              for
                paramType <- convertParamElementType(paramElem, env)
                paramType2 = ExprToHolesConverter(context)(exprConverter.exprContext).processWrapExpr(paramType)
                paramVar = exprConverter.exprContext.ParameterVariable(owner, index, paramType2, param.isErased, Some(paramElem.name))

                (next, env) <- impl(addVariableToEnv(env, listType, paramVar))(tail)(index + 1)
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
                paramType2 = ExprToHolesConverter(context)(exprConverter.exprContext).processWrapExpr(paramType)
                paramVar = exprConverter.exprContext.ParameterVariable(owner, index, paramType2, param.isErased, None)
                paramVarElements = paramElementTypes
                  .zipWithIndex
                  .map {
                    case ((name, t), i) =>
                      val t2 = ExprToHolesConverter(context)(exprConverter.exprContext).processWrapExpr(t)
                      name -> exprConverter.ParameterVariableElement(paramVar, i, t2)
                  }
                  .toMap

                (next, env) <- impl(env.withScope(_.addVariable(paramVar).addParameterVariableElements(paramVarElements)))(tail)(index + 1)
              yield (Signature.Parameter(param.listType, param.isErased, None, paramType, next), env)
          }

        case _ =>
          createResult(env, opt).map { res =>
              (Signature.Result(res), env)
          }

    impl(env)(parameters)(0)
  end create

  def createTraitResult
  (context: Context)
  (exprConverter: ExpressionConverter & HasContext[context.type])
  (stmt: parser.TraitDeclarationStmt)
  (env: exprConverter.Env, opt: exprConverter.ExprOptions)
  : context.Comp[context.ExprContext.TraitResult] =
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

    def baseTypes(env: Env): Comp[Seq[context.ExprContext.ArExpr[context.ExprContext.ExprConstructor.TraitType]]] =
      for
        (baseTraits, env) <- stmt.baseType match {
          case Some(baseType) =>
            for {
              baseTypeResult <- exprConverter.convertExpr(baseType).check(env, opt.forTypeExpr, traitType)
              baseTraits <- impl(baseTypeResult.expr, Seq.empty)
            } yield (baseTraits, baseTypeResult.env)

          case None =>
            ZIO.succeed((Seq.empty, env))
        }

        envCell <- Ref.make(env)

        baseTraits2 <- ZIO.foreach(baseTraits) { baseTrait =>
          for
            env <- envCell.get
            (res, env) <- exprConverter.resolveHolesTraitType(env, baseTrait)
            _ <- envCell.set(env)
          yield res
        }

      yield baseTraits2

    for
      (traitType2, env) <- exprConverter.resolveHoles(env, traitType)
      baseTraits <- baseTypes(env).memoize
    yield context.ExprContext.TraitResult(traitType2, baseTraits)
  end createTraitResult

  def createClassResult
  (context: Context)
  (exprConverter: ExpressionConverter & HasContext[context.type])
  (stmt: parser.ClassDeclarationStmt)
  (env: exprConverter.Env, opt: exprConverter.ExprOptions)
  : context.Comp[context.ExprContext.ClassResult] =
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

    def baseTypes(env: Env): Comp[(Option[context.ExprContext.ArExpr[context.ExprContext.ExprConstructor.ClassType]], Seq[context.ExprContext.ArExpr[context.ExprContext.ExprConstructor.TraitType]])] =
      for
        (baseClass, baseTraits, env) <-
          stmt.baseType match {
            case Some(baseType) =>
              for {
                exprConverter.ExprResult(baseTypeExpr, env) <- exprConverter.convertExpr(baseType).check(env, opt.forTypeExpr, classType)
                (baseClass, baseTraits) <- impl(baseTypeExpr, None, Seq.empty)
              } yield (baseClass, baseTraits, env)

            case None =>
              ZIO.succeed((None, Seq.empty, env))
          }

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

      yield (baseClass2, baseTraits2)

    for
      (classType2, env) <- exprConverter.resolveHoles(env, classType)

      baseTypesPair <- baseTypes(env).memoize

    yield context.ExprContext.ClassResult(classType2, baseTypesPair.map { _._1 }, baseTypesPair.map { _._2 })
  end createClassResult

  def createFunctionResult
  (context: Context)
  (exprConverter: ExpressionConverter & HasContext[context.type])
  (owner: exprConverter.exprContext.ParameterVariableOwner)
  (returnTypeExpr: WithSource[parser.ReturnTypeSpecifier])
  (env: exprConverter.Env, opt: exprConverter.ExprOptions)
  : context.Comp[context.ExprContext.FunctionResult] =
    Ref.make(env).flatMap { envRef =>
      def convertExprResolved(expr: WithSource[parser.Expr]): context.Comp[context.ExprContext.WrapExpr] =
        for
          env <- envRef.get
          exprConverter.ExprResult(convExpr, env) <- exprConverter.convertExpr(expr).check(env, opt.forTypeExpr, exprConverter.anyType)
          (resolvedExpr, env) <- exprConverter.resolveHoles(env, convExpr)
          _ <- envRef.set(env)
        yield resolvedExpr

      for
        returnType <- convertExprResolved(returnTypeExpr.value.returnType)
        _ <- envRef.update { env =>
          val returnType2 = ExprToHolesConverter(context)(exprConverter.exprContext).processWrapExpr(returnType)
          val resultVar = exprConverter.exprContext.FunctionResultVariable(owner, returnType2)
          env.withScope(_.addVariable(resultVar))
        }
        ensuresClauses <- ZIO.foreach(returnTypeExpr.value.ensuresClauses)(convertExprResolved)
      yield context.ExprContext.FunctionResult(returnType, ensuresClauses)
    }



  def createAccessToken(exprConverter: ExpressionConverter)(owner: exprConverter.exprContext.ParameterVariableOwner): exprConverter.AccessToken =
    import exprConverter.*
    import exprContext.*
    import dev.argon.compiler.definitions.*

    owner match {
      case owner: (ArMethodC & HasContext[context.type]) =>
        owner.owner match {
          case OwnedByClassC(arClass, _, _) => createAccessToken(exprConverter)(arClass)
          case OwnedByClassStaticC(arClass, _, _) => createAccessToken(exprConverter)(arClass)
          case OwnedByTraitC(arTrait, _, _) => createAccessToken(exprConverter)(arTrait)
          case OwnedByTraitStaticC(arTrait, _, _) => createAccessToken(exprConverter)(arTrait)
        }

      case owner: (ClassConstructorC & HasContext[context.type]) =>
        createAccessToken(exprConverter)(owner.owner.arClass)

      case owner: (ArFuncC & HasContext[context.type]) =>
        AccessToken.ModuleToken(owner.owner.module)

      case owner: (ArClassC & HasContext[context.type]) =>
        AccessToken.Multi(Set(
          AccessToken.ModuleToken(owner.owner.module),
          AccessToken.ClassToken(owner),
        ))

      case owner: (ArTraitC & HasContext[context.type]) =>
        AccessToken.Multi(Set(
          AccessToken.ModuleToken(owner.owner.module),
          AccessToken.TraitToken(owner),
        ))
    }
  end createAccessToken


}
