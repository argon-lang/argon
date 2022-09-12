package dev.argon.plugins.source

import dev.argon.compiler.*
import dev.argon.compiler.definitions.*
import dev.argon.compiler.expr.{ArgonExprContext, ExprToHolesConverter}
import dev.argon.util.{*, given}
import dev.argon.compiler.signature.Signature
import dev.argon.parser
import dev.argon.parser.{ClassConstructorDeclarationStmt, IdentifierExpr}
import zio.*
import zio.stm.*

object SourceClassConstructor {

  def make[TOwner]
  (ctx: Context)
  (exprConverter: ExpressionConverter & HasContext[ctx.type])
  (outerEnv: exprConverter.Env)
  (ctorOwnership: TOwner & ClassConstructorC.Ownership[ctx.type, true])
  (stmt: ClassConstructorDeclarationStmt)
  : ctx.Comp[ClassConstructorC & HasContext[ctx.type] & HasImplementation[true] & HasOwner[TOwner]] =
    for
      methodId <- UniqueIdentifier.make
      sigCell <- MemoCell.make[ctx.Env, ctx.Error, (Signature[ctx.ExprContext.WrapExpr, Unit], exprConverter.Env)]
      implCell <- MemoCell.make[ctx.Env, ctx.Error, ClassConstructorImplementationC & HasContext[ctx.type]]

    yield (new ClassConstructorC {
      override val context: ctx.type = ctx

      import context.ExprContext.{WrapExpr, ArExpr, ExprConstructor}

      override val owner: ctorOwnership.type = ctorOwnership
      override val id: UniqueIdentifier = methodId

      override val purity: Boolean = stmt.purity

      private def sigEnv: Comp[(Signature[WrapExpr, Unit], exprConverter.Env)] =
        sigCell.get(
          SignatureUtil.create(context)(exprConverter)(this)(outerEnv)(stmt.parameters)(
            (_, _) => ZIO.unit
          )
        )

      private def innerEnv: Comp[exprConverter.Env] =
        sigEnv.map { _._2 }

      override def signatureUnsubstituted: Comp[Signature[WrapExpr, Unit]] =
        sigEnv.map { _._1 }

      override type IsImplementation = true

      override def implementation: Comp[ClassConstructorImplementation] =
        implCell.get(
          stmt.body.value match {
            case Seq(WithSource(parser.ExternExpr(specifier), location)) =>
              val tube = ArMethodC.getOwningModule(owner).tube
              context.getExternClassConstructorImplementation(tube.options, specifier)
                .mapBoth(
                  {
                    case Some(e) => e
                    case None => DiagnosticError.ExternMethodNotFound(DiagnosticSource.Location(location), specifier)
                  },
                  extern => new ClassConstructorImplementationC.External {
                    override val context: ctx.type = ctx
                    override val name: String = specifier
                    override val impl: context.ExternClassConstructorImplementation = extern
                  }
                )

            case _ =>
              import exprConverter.{Env, ExprResult}
              type FieldInitializationStatement = ClassConstructorImplementationC.FieldInitializationStatement & HasContext[context.type]
              type BaseClassConstructorCallStatement = ClassConstructorImplementationC.BaseClassConstructorCallStatement & HasContext[context.type]

              final case class UnresolvedFieldInit(field: context.ExprContext.MemberVariable, fieldConv: exprConverter.exprContext.MemberVariable, value: exprConverter.exprContext.WrapExpr)

              val opt = exprConverter.ExprOptions(
                purity = stmt.purity,
                accessToken = SignatureUtil.createAccessToken(exprConverter)(this),
                allowAbstractConstructorCall = false,
                allowErased = false,
                postconditions = None,
              )

              def stmtsWithLocation(stmts: Seq[WithSource[parser.Stmt]]): Option[WithSource[Seq[WithSource[parser.Stmt]]]] =
                for
                  head <- stmts.headOption
                  last <- stmts.lastOption
                yield WithSource(stmts, SourceLocation.merge(head.location, last.location))

              def divideBody
              (
                body: Seq[WithSource[parser.Stmt]],
                preBaseCall: Seq[Either[Seq[WithSource[parser.Stmt]], WithSource[parser.FieldInitializationStmt]]],
                postFieldInit: Seq[WithSource[parser.Stmt]],
              ): (Seq[Either[Seq[WithSource[parser.Stmt]], WithSource[parser.FieldInitializationStmt]]], Option[WithSource[parser.InitializeStmt]], Seq[WithSource[parser.Stmt]]) =
                body match {
                  case WithSource(init: parser.InitializeStmt, initLoc) +: tail =>
                    (
                      preBaseCall :+ Left(postFieldInit),
                      Some(WithSource(init, initLoc)),
                      tail
                    )

                  case WithSource(fieldInit: parser.FieldInitializationStmt, initLoc) +: tail =>
                    divideBody(tail, preBaseCall :+ Left(postFieldInit) :+ Right(WithSource(fieldInit, initLoc)), Seq())

                  case head +: tail =>
                    divideBody(tail, preBaseCall, postFieldInit :+ head)

                  case _ =>
                    (preBaseCall, None, postFieldInit)
                }

              def convertStmtBlock(env: Env, stmts: Seq[WithSource[parser.Stmt]]): Comp[ExprResult] =
                stmtsWithLocation(stmts).fold(ZIO.succeed(ExprResult(exprConverter.unitValue, env))) { stmts =>
                  exprConverter.convertStmtList(stmts).check(env, opt, exprConverter.unitType)
                }

              def convertPreBaseCallBlocks(env: Env, stmts: Seq[Either[Seq[WithSource[parser.Stmt]], WithSource[parser.FieldInitializationStmt]]]): Comp[(Seq[exprConverter.exprContext.WrapExpr | UnresolvedFieldInit], Env)] =
                for
                  fields <- owner.arClass.fields
                  initializedFields <- TSet.empty[context.ExprContext.MemberVariable].commit
                  result <- ZIO.foldLeft(stmts)((Seq.empty[exprConverter.exprContext.WrapExpr | UnresolvedFieldInit], env)) {
                    case ((acc, env), Left(stmts)) =>
                      convertStmtBlock(env, stmts)
                        .map { case ExprResult(expr, env) => (acc :+ expr, env) }

                    case ((acc, env), Right(fieldInit)) =>
                      for
                        field <- ZIO.fromEither(
                          fields.find(_.name.get == fieldInit.value.name)
                            .toRight { DiagnosticError.FieldNotFound(DiagnosticSource.Location(fieldInit.location), fieldInit.value.name) }
                        )
                        _ <- (
                          ZSTM.fail(DiagnosticError.FieldReinitialized(DiagnosticSource.Location(fieldInit.location))).whenSTM(initializedFields.contains(field)) *>
                            initializedFields.put(field)
                        ).commit

                        fieldConv = ExprToHolesConverter(context)(exprConverter.exprContext).processMemberVariable(field)
                        (ExprResult(value, env)) <- exprConverter.convertExpr(fieldInit.value.value).check(env, opt, fieldConv.varType)
                      yield (acc :+ UnresolvedFieldInit(field, fieldConv, value), env)
                  }
                  _ <- ZIO.foreachDiscard(fields) { field =>
                    ZIO.fail(DiagnosticError.FieldNotInitialized(DiagnosticSource.Location(stmt.newLocation)))
                      .unlessZIO(initializedFields.contains(field).commit)
                  }
                yield result


              def convertBaseCall(env: Env)(stmt: Option[WithSource[parser.InitializeStmt]]): Comp[(Option[exprConverter.exprContext.ArExpr[exprConverter.exprContext.ExprConstructor.ClassConstructorCall]], exprConverter.exprContext.LocalVariable, Env)] =
                def extractEnv[T](env: Env)(opt: Option[(T, Env)]): (Option[T], Env) =
                  opt match
                    case Some((t, env)) => (Some(t), env)
                    case None => (None, env)
                  end match

                def createClassType(sig: Signature[WrapExpr, context.ExprContext.ClassResult]): exprConverter.exprContext.WrapExpr =
                  exprConverter.exprContext.WrapExpr.OfExpr(
                    exprConverter.exprContext.ArExpr(
                      exprConverter.exprContext.ExprConstructor.ClassType(owner.arClass),
                      sig.parameters.zipWithIndex.map { case (param, i) =>
                        val paramType = ExprToHolesConverter(context)(exprConverter.exprContext).processWrapExpr(param.paramType)

                        val paramVar = exprConverter.exprContext.ParameterVariable(owner.arClass, i, paramType, param.isErased, param.name)

                        exprConverter.exprContext.WrapExpr.OfExpr(
                          exprConverter.exprContext.ArExpr(
                            exprConverter.exprContext.ExprConstructor.LoadVariable(paramVar),
                            EmptyTuple
                          )
                        )
                      }
                    )
                  )

                def createBaseCall(env: Env)(sig: Signature[WrapExpr, context.ExprContext.ClassResult])(valueExpr: WithSource[parser.Expr]): Comp[(exprConverter.exprContext.ArExpr[exprConverter.exprContext.ExprConstructor.ClassConstructorCall], Env)] =
                  exprConverter.convertExpr(valueExpr).check(env, opt.copy(allowAbstractConstructorCall = true), exprConverter.anyType)
                    .flatMap {
                      case ExprResult(exprConverter.exprContext.WrapExpr.OfExpr(baseCall), env) =>
                        baseCall.constructor match {
                          case ctor: (baseCall.constructor.type & exprConverter.exprContext.ExprConstructor.ClassConstructorCall) =>

                            def typeMatches(env: Env)(t: exprConverter.exprContext.WrapExpr): Comp[Option[Env]] =
                              val (instanceType, _) = baseCall.getArgs(ctor)
                              exprConverter.isSameType(env, exprConverter.exprContext.WrapExpr.OfExpr(instanceType), t)
                            end typeMatches

                            val classType = createClassType(sig)

                            typeMatches(env)(classType)
                              .flatMap {
                                case Some(env) => ZIO.succeed(Some(env))
                                case None =>
                                  sig.unsubstitutedResult.baseClass.flatMap { baseClass =>
                                    ZIO.foreach(baseClass) { baseClass =>
                                      val baseClassConv = ExprToHolesConverter(context)(exprConverter.exprContext).processClassType(baseClass)
                                      typeMatches(env)(exprConverter.exprContext.WrapExpr.OfExpr(baseClassConv))
                                    }.map(_.flatten)
                                  }
                              }
                              .flatMap {
                                case Some(env) => ZIO.succeed((exprConverter.exprContext.ArExpr(ctor, baseCall.getArgs(ctor)), env))
                                case None => ???
                              }

                          case _ =>
                            ???
                        }

                      case _ => ???
                    }

                def createInstanceVar(env: Env)(sig: Signature[WrapExpr, context.ExprContext.ClassResult]): Comp[exprConverter.exprContext.LocalVariable] =
                  for
                    id <- UniqueIdentifier.make
                  yield exprConverter.exprContext.LocalVariable(
                    id,
                    createClassType(sig),
                    name = stmt.flatMap { _.value.name },
                    isMutable = false,
                    isErased = false,
                  )

                for
                  sig <- owner.arClass.signature

                  (baseClassOpt, env) <-
                    ZIO.foreach(stmt) { stmt =>
                      ZIO.foreach(stmt.value.value)(createBaseCall(env)(sig))
                    }
                      .map(_.flatten)
                      .map(extractEnv(env))

                  instanceVariable <- createInstanceVar(env)(sig)

                  _ <- baseClassOpt match {
                    case Some(_) => ZIO.unit
                    case None =>
                      sig.unsubstitutedResult.baseClass.map { baseClass =>
                        if baseClass.isEmpty then
                          (None, env)
                        else
                          ???
                      }
                  }
                yield (baseClassOpt, instanceVariable, env)
              end convertBaseCall

              def resolvePreBaseCallBlocks(env: Env, stmts: Seq[exprConverter.exprContext.WrapExpr | UnresolvedFieldInit]): Comp[(Seq[Either[WrapExpr, FieldInitializationStatement]], Env)] =
                ZIO.foldLeft(stmts)((Seq.empty[Either[WrapExpr, FieldInitializationStatement]], env)) {
                  case ((acc, env), expr: exprConverter.exprContext.WrapExpr) =>
                    exprConverter.resolveHoles(env, expr)
                      .map { case (resolvedExpr, env)  => (acc :+ Left(resolvedExpr), env) }

                  case ((acc, env), fieldInit: UnresolvedFieldInit) =>
                    for
                      (resolvedValue, env) <- exprConverter.resolveHoles(env, fieldInit.value)

                      fieldInitResolved = new ClassConstructorImplementationC.FieldInitializationStatement {
                        override val context: ctx.type = ctx
                        override val field: context.ExprContext.MemberVariable = fieldInit.field
                        override val value: context.ExprContext.WrapExpr = resolvedValue
                      }

                    yield (acc :+ Right(fieldInitResolved), env)
                }

              val (preInitStmts, initStmt, postInitStmts) = divideBody(stmt.body.value, Seq.empty, Seq.empty)

              for
                env <- innerEnv

                (preInitExpr, env) <- convertPreBaseCallBlocks(env, preInitStmts)
                (baseCallOpt, instanceVariable, env) <- convertBaseCall(env)(initStmt)
                (ExprResult(postInitExpr, env)) <- convertStmtBlock(env, postInitStmts)

                (preInitResolved, env) <- resolvePreBaseCallBlocks(env, preInitExpr)
                (baseCallResolvedOpt, env) <- ZIO.foreach(baseCallOpt) { baseCall =>
                  exprConverter.resolveHolesClassConstructorCall(env, baseCall)
                }.map {
                  case Some((baseCall, env)) => (Some(baseCall), env)
                  case None => (None, env)
                }
                (instanceVariableResolved, env) <- exprConverter.resolveHolesLocalVariable(env, instanceVariable)
                (postInitResolved, _) <- exprConverter.resolveHoles(env, postInitExpr)
              yield new ClassConstructorImplementationC.ExpressionBody {
                override val context: ctx.type = ctx

                override val preInitialization: Seq[Either[WrapExpr, FieldInitializationStatement]] = preInitResolved

                override val baseConstructorCall: BaseClassConstructorCallStatement =
                  new ClassConstructorImplementationC.BaseClassConstructorCallStatement {
                    override val context: ctx.type = ctx
                    override val baseCall: Option[ArExpr[ExprConstructor.ClassConstructorCall]] = baseCallResolvedOpt
                    override val instanceVariable: context.ExprContext.LocalVariable = instanceVariableResolved
                  }

                override val postInitialization: WrapExpr = postInitResolved
              }
          }
        )


    } : ClassConstructorC & HasContext[ctx.type] & HasImplementation[true] & HasOwner[TOwner])

}
