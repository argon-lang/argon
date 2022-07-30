package dev.argon.plugins.source

import dev.argon.compiler.*
import dev.argon.compiler.definitions.*
import dev.argon.compiler.expr.*
import dev.argon.compiler.signature.Signature
import dev.argon.parser
import dev.argon.parser.{FunctionDeclarationStmt, IdentifierExpr}
import dev.argon.util.*
import zio.*

object SourceFunction {

  def make[TOwner]
  (ctx: Context)
  (exprConverter: ExpressionConverter with HasContext[ctx.type])
  (outerEnv: exprConverter.Env)
  (functionOwner: TOwner)
  (stmt: FunctionDeclarationStmt)
  : ctx.Comp[ArFuncC with HasContext[ctx.type] with HasDeclaration[true] with HasOwner[TOwner]] =
    for
      funcId <- UniqueIdentifier.make
      sigCell <- MemoCell.make[ctx.Env, ctx.Error, Signature[ctx.ExprContext.WrapExpr, ctx.ExprContext.WrapExpr]]

    yield new ArFuncC {
      override val context: ctx.type = ctx
      import context.ExprContext.{WrapExpr, ArExpr, ExprConstructor}

      override val id: UniqueIdentifier = funcId

      override type IsDeclaration = true

      override def signature: Comp[Signature[WrapExpr, WrapExpr]] =
        sigCell.get(
          SignatureUtil.create(context)(exprConverter)(this)(outerEnv)(stmt.parameters)(
            SignatureUtil.createFunctionResult(context)(exprConverter)(stmt.returnType)
          )
            .flatMap { (sig, env) =>
              SignatureUtil.resolveHolesSig(context)(exprConverter)(env)(exprConverter.functionSigHandler)(sig)
                .map { case (sig, _) => sig: Signature[WrapExpr, WrapExpr] }
            }
        )

      override val owner: TOwner = functionOwner
    }

}
