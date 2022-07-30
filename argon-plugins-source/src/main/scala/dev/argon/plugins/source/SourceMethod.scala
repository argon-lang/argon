package dev.argon.plugins.source

import dev.argon.compiler.*
import dev.argon.compiler.definitions.*
import dev.argon.util.*
import dev.argon.compiler.signature.Signature
import dev.argon.parser
import dev.argon.parser.{MethodDeclarationStmt, IdentifierExpr}
import zio.*

object SourceMethod {

  def make[TOwner]
  (ctx: Context)
  (exprConverter: ExpressionConverter with HasContext[ctx.type])
  (outerEnv: exprConverter.Env)
  (methodOwner: TOwner & ArMethodC.Ownership[ctx.type])
  (stmt: MethodDeclarationStmt)
  : ctx.Comp[ArMethodC with HasContext[ctx.type] with HasDeclaration[true] with HasOwner[TOwner]] =
    for
      methodId <- UniqueIdentifier.make
      sigCell <- MemoCell.make[ctx.Env, ctx.Error, Signature[ctx.ExprContext.WrapExpr, ctx.ExprContext.WrapExpr]]
      implCell <- MemoCell.make[ctx.Env, ctx.Error, MethodImplementationC with HasContext[ctx.type]]

    yield new ArMethodC {
      override val context: ctx.type = ctx
      import context.ExprContext.{WrapExpr, ArExpr, ExprConstructor}

      override val owner: methodOwner.type = methodOwner
      override val id: UniqueIdentifier = methodId

      override def isAbstract: Boolean = stmt.modifiers.exists(_.value == parser.AbstractModifier)
      override def isImplicitOverride: Boolean = stmt.modifiers.exists(_.value == parser.OverrideModifier)
      override def isVirtual: Boolean = stmt.modifiers.exists(_.value == parser.VirtualModifier)
      override def isFinal: Boolean = stmt.modifiers.exists(_.value == parser.FinalModifier)

      override def signatureUnsubstituted: Comp[Signature[WrapExpr, WrapExpr]] =
        sigCell.get(
          SignatureUtil.create(context)(exprConverter)(this)(outerEnv)(stmt.parameters)(
            SignatureUtil.createFunctionResult(context)(exprConverter)(stmt.returnType)
          )
            .flatMap { (sig, env) =>
              SignatureUtil.resolveHolesSig(context)(exprConverter)(env)(exprConverter.functionSigHandler)(sig)
                .map { case (sig, _) => sig: Signature[WrapExpr, WrapExpr] }
            }
        )

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
              ???
          }
        )


    }

}
