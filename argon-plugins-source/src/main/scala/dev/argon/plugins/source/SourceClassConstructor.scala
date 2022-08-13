package dev.argon.plugins.source

import dev.argon.compiler.*
import dev.argon.compiler.definitions.*
import dev.argon.compiler.expr.ArgonExprContext
import dev.argon.util.{*, given}
import dev.argon.compiler.signature.Signature
import dev.argon.parser
import dev.argon.parser.{ClassConstructorDeclarationStmt, IdentifierExpr}
import zio.*

object SourceClassConstructor {

  def make[TOwner]
  (ctx: Context)
  (exprConverter: ExpressionConverter & HasContext[ctx.type])
  (outerEnv: exprConverter.Env)
  (ctorOwnership: TOwner & ClassConstructorC.Ownership[ctx.type])
  (stmt: ClassConstructorDeclarationStmt)
  : ctx.Comp[ClassConstructorC & HasContext[ctx.type] & HasDeclaration[true] & HasOwner[TOwner]] =
    for
      methodId <- UniqueIdentifier.make
      sigCell <- MemoCell.make[ctx.Env, ctx.Error, (Signature[ctx.ExprContext.WrapExpr, Unit], exprConverter.Env)]
      implCell <- MemoCell.make[ctx.Env, ctx.Error, ClassConstructorImplementationC & HasContext[ctx.type]]

    yield (new ClassConstructorC {
      override val context: ctx.type = ctx

      import context.ExprContext.{WrapExpr, ArExpr, ExprConstructor}

      override val owner: ctorOwnership.type = ctorOwnership
      override val id: UniqueIdentifier = methodId


      private def sigEnv: Comp[(Signature[WrapExpr, Unit], exprConverter.Env)] =
        sigCell.get(
          SignatureUtil.create(context)(exprConverter)(this)(outerEnv)(stmt.parameters)(
            _ => ZIO.unit
          )
        )

      private def innerEnv: Comp[exprConverter.Env] =
        sigEnv.map { _._2 }

      override def signatureUnsubstituted: Comp[Signature[WrapExpr, Unit]] =
        sigEnv.map { _._1 }

      override type IsDeclaration = true

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
                    override val impl: context.ExternClassConstructorImplementation = extern
                  }
                )

            case _ =>
              import exprConverter.{Env, ExprResult}
              type FieldInitializationStatement = ClassConstructorImplementationC.FieldInitializationStatement & HasContext[context.type]
              type BaseClassConstructorCallStatement = ClassConstructorImplementationC.BaseClassConstructorCallStatement & HasContext[context.type]

              final case class UnresolvedFieldInit(field: context.ExprContext.MemberVariable, fieldConv: exprConverter.exprContext.MemberVariable, value: exprConverter.exprContext.WrapExpr)

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
                  exprConverter.convertStmtList(stmts).check(env, exprConverter.unitType)
                }

              def convertPreBaseCallBlocks(env: Env, stmts: Seq[Either[Seq[WithSource[parser.Stmt]], WithSource[parser.FieldInitializationStmt]]]): Comp[(Seq[exprConverter.exprContext.WrapExpr | UnresolvedFieldInit], Env)] =
                ZIO.foldLeft(stmts)((Seq.empty[exprConverter.exprContext.WrapExpr | UnresolvedFieldInit], env)) {
                  case ((acc, env), Left(stmts)) =>
                    convertStmtBlock(env, stmts)
                      .map { case ExprResult(expr, env) => (acc :+ expr, env) }

                  case ((acc, env), Right(fieldInit)) =>
                    for
                      fields <- owner.arClass.fields
                      field <- ZIO.fromEither(
                        fields.find(_.name.get == fieldInit.value.name)
                          .toRight { DiagnosticError.FieldNotFound(DiagnosticSource.Location(fieldInit.location), fieldInit.value.name) }
                      )
                      fieldConv = ArgonExprContext.convertMemberVariable[Id](context)(context.ExprContext, exprConverter.exprContext)(identity)(field)
                      (ExprResult(value, env)) <- exprConverter.convertExpr(fieldInit.value.value).check(env, fieldConv.varType)
                    yield (acc :+ UnresolvedFieldInit(field, fieldConv, value), env)
                }

              def convertBaseCall(env: Env)(stmt: WithSource[parser.InitializeStmt]): Comp[(exprConverter.exprContext.ArExpr[exprConverter.exprContext.ExprConstructor.ClassConstructorCall], Env)] =
                ???

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
                (baseCallOpt, env) <- ZIO.foreach(initStmt)(convertBaseCall(env)).map {
                  case Some((baseCall, env)) => (Some(baseCall), env)
                  case None => (None, env)
                }
                (ExprResult(postInitExpr, env)) <- convertStmtBlock(env, postInitStmts)

                (preInitResolved, env) <- resolvePreBaseCallBlocks(env, preInitExpr)
                (baseCallResolvedOpt, env) <- ZIO.foreach(baseCallOpt) { baseCall =>
                  ??? : Comp[(BaseClassConstructorCallStatement, Env)]
                }.map {
                  case Some((baseCall, env)) => (Some(baseCall), env)
                  case None => (None, env)
                }
                (postInitResolved, _) <- exprConverter.resolveHoles(env, postInitExpr)
              yield new ClassConstructorImplementationC.ExpressionBody {
                override val context: ctx.type = ctx
                override val preInitialization: Seq[Either[WrapExpr, FieldInitializationStatement]] = preInitResolved
                override val baseConstructorCall: Option[BaseClassConstructorCallStatement] = baseCallResolvedOpt
                override val postInitialization: WrapExpr = postInitResolved
              }
          }
        )


    } : ClassConstructorC & HasContext[ctx.type] & HasDeclaration[true] & HasOwner[TOwner])

}
