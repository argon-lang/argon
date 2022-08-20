package dev.argon.plugins.source

import dev.argon.compiler.*
import dev.argon.compiler.definitions.*
import dev.argon.compiler.expr.{ArgonExprContext, ExprToHolesConverter}
import dev.argon.util.{*, given}
import dev.argon.compiler.signature.Signature
import dev.argon.parser
import dev.argon.parser.{IdentifierExpr, MethodDeclarationStmt}
import zio.*

object SourceMethod {

  def make[TOwner]
  (ctx: Context)
  (exprConverter: ExpressionConverter & HasContext[ctx.type])
  (outerEnv: exprConverter.Env)
  (methodOwner: TOwner & ArMethodC.Ownership[ctx.type])
  (stmt: MethodDeclarationStmt)
  : ctx.Comp[ArMethodC & HasContext[ctx.type] & HasDeclaration[true] & HasOwner[TOwner]] =
    for
      methodId <- UniqueIdentifier.make
      sigCell <- MemoCell.make[ctx.Env, ctx.Error, (Signature[ctx.ExprContext.WrapExpr, ctx.ExprContext.WrapExpr], exprConverter.Env)]
      implCell <- MemoCell.make[ctx.Env, ctx.Error, MethodImplementationC & HasContext[ctx.type]]

    yield new ArMethodC {
      override val context: ctx.type = ctx
      import context.ExprContext.{WrapExpr, ArExpr, ExprConstructor}

      override val owner: methodOwner.type = methodOwner
      override val id: UniqueIdentifier = methodId

      override def isAbstract: Boolean = stmt.modifiers.exists(_.value == parser.AbstractModifier)
      override def isImplicitOverride: Boolean = stmt.modifiers.exists(_.value == parser.OverrideModifier)
      override def isVirtual: Boolean = stmt.modifiers.exists(mod => mod.value == parser.VirtualModifier || mod.value == parser.AbstractModifier)
      override def isFinal: Boolean = stmt.modifiers.exists(_.value == parser.FinalModifier)

      private def sigEnv: Comp[(Signature[WrapExpr, WrapExpr], exprConverter.Env)] =
        sigCell.get(
          SignatureUtil.create(context)(exprConverter)(this)(outerEnv)(stmt.parameters)(
            SignatureUtil.createFunctionResult(context)(exprConverter)(stmt.returnType)
          )
        )

      private def innerEnv: Comp[exprConverter.Env] =
        sigEnv.map { _._2 }

      override def signatureUnsubstituted: Comp[Signature[WrapExpr, WrapExpr]] =
        sigEnv.map { _._1 }

      override type IsDeclaration = true
      override def implementation: Comp[MethodImplementation] =
        implCell.get(
          stmt.body match {
            case None =>
              ZIO.succeed(new MethodImplementationC.Abstract {
                override val context: ctx.type = ctx
              })

            case Some(WithSource(parser.ExternExpr(specifier), location)) =>
              val tube = ArMethodC.getOwningModule(owner).tube
              context.getExternMethodImplementation(tube.options, specifier)
                .mapBoth(
                  {
                    case Some(e) => e
                    case None => DiagnosticError.ExternMethodNotFound(DiagnosticSource.Location(location), specifier)
                  },
                  extern => new MethodImplementationC.External {
                    override val context: ctx.type = ctx
                    override val impl: context.ExternMethodImplementation = extern
                  }
                )

            case Some(expr) =>
              for
                sig <- signatureUnsubstituted
                returnType = ExprToHolesConverter(context)(exprConverter.exprContext).processWrapExpr(sig.unsubstitutedResult)
                env <- innerEnv
                bodyResult <- exprConverter.convertExpr(expr).check(env, returnType)
                resolvedBody <- exprConverter.resolveHoles(bodyResult.env, bodyResult.expr)
              yield new MethodImplementationC.ExpressionBody {
                override val context: ctx.type = ctx
                override val body: WrapExpr = resolvedBody._1
              }
          }
        )


    }

}
