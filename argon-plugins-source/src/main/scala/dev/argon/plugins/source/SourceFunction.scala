package dev.argon.plugins.source

import dev.argon.compiler.{definitions, *}
import dev.argon.compiler.definitions.*
import dev.argon.compiler.expr.*
import dev.argon.compiler.signature.Signature
import dev.argon.parser
import dev.argon.parser.{FunctionDeclarationStmt, IdentifierExpr}
import dev.argon.util.{*, given}
import zio.*

object SourceFunction {

  def make[TOwner]
  (ctx: Context)
  (tubeOptions: ctx.Options)
  (exprConverter: ExpressionConverter & HasContext[ctx.type])
  (outerEnv: exprConverter.Env)
  (functionOwner: TOwner & ArFuncC.Ownership[ctx.type, true])
  (stmt: FunctionDeclarationStmt)
  : ctx.Comp[ArFuncC & HasContext[ctx.type] & HasImplementation[true] & HasOwner[TOwner]] =
    for
      funcId <- UniqueIdentifier.make

      innerEnvCell <- MemoCell.make[ctx.Env, ctx.Error, exprConverter.Env]

      sigCell <- MemoCell.make[ctx.Env, ctx.Error, (Signature[ctx.ExprContext.WrapExpr, ctx.ExprContext.FunctionResult], exprConverter.Env)]
      implCell <- MemoCell.make[ctx.Env, ctx.Error, FunctionImplementationC & HasContext[ctx.type]]

    yield new ArFuncC {
      override val context: ctx.type = ctx
      import context.ExprContext.{WrapExpr, ArExpr, ExprConstructor, FunctionResult}

      override val id: UniqueIdentifier = funcId
      override val owner: functionOwner.type = functionOwner

      override type IsImplementation = true

      private def sigEnv: Comp[(Signature[WrapExpr, FunctionResult], exprConverter.Env)] =
        sigCell.get(
          SignatureUtil.create(context)(exprConverter)(this)(outerEnv)(stmt.parameters)(
            SignatureUtil.createFunctionResult(context)(exprConverter)(this)(stmt.returnType)
          )
        )


      private def innerEnv: Comp[exprConverter.Env] =
        sigEnv.map { _._2 }

      override def signature: Comp[Signature[WrapExpr, FunctionResult]] =
        sigEnv.map { _._1 }
      
      override def purity: Boolean = stmt.purity
      override def isProof: Boolean = stmt.modifiers.exists { _.value == parser.ProofModifier }
      override def isErased: Boolean = stmt.modifiers.exists { _.value == parser.ErasedModifier }
      override def isInline: Boolean = stmt.modifiers.exists { _.value == parser.InlineModifier }

      override def implementation: Comp[FunctionImplementation] =
        implCell.get(
          stmt.body match {
            case WithLocation(parser.ExternExpr(specifier), location) =>
              context.getExternFunctionImplementation(tubeOptions, specifier)
                .mapBoth(
                  {
                    case Some(e) => e
                    case None => DiagnosticError.ExternFunctionNotFound(DiagnosticSource.Location(location), specifier)
                  },
                  extern => new FunctionImplementationC.External {
                    override val context: ctx.type = ctx
                    override val name: String = specifier
                    override val impl: context.ExternFunctionImplementation = extern
                  }
                )

            case expr =>
              for
                sig <- signature
                returnType = ExprToHolesConverter(context)(exprConverter.exprContext).processWrapExpr(sig.unsubstitutedResult.returnType)
                env <- innerEnv
                opt = exprConverter.ExprOptions(
                  purity = stmt.purity,
                  accessToken = SignatureUtil.createAccessToken(exprConverter)(this),
                  allowAbstractConstructorCall = false,
                  allowErased = false,
                  postconditions = Some(exprConverter.Postconditions(
                    resultVar = exprConverter.exprContext.FunctionResultVariable(this, returnType),
                    conditions = sig.unsubstitutedResult.ensuresClauses.map(ExprToHolesConverter(context)(exprConverter.exprContext).processWrapExpr)
                  )),
                )
                bodyResult <- exprConverter.convertExpr(expr).check(env, opt, returnType)
                (resolvedBody, env) <- exprConverter.resolveHoles(bodyResult.env, bodyResult.expr)

              yield new FunctionImplementationC.ExpressionBody {
                override val context: ctx.type = ctx
                override val body: WrapExpr = resolvedBody
              }
          }
        )


      override def maybeImplementation: Comp[Option[FunctionImplementation]] =
        implementation.asSome

      override def validate: Comp[Unit] =
        for
          _ <- ZIO.fail(DiagnosticError.ProofMustBePure(DiagnosticSource.Location(stmt.name.location))).when(isProof && !purity)
          _ <- ZIO.fail(DiagnosticError.ErasedMustBePure(DiagnosticSource.Location(stmt.name.location))).when(isErased && !purity)
        yield ()
    }

}
