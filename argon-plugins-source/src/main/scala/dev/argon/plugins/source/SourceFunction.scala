package dev.argon.plugins.source

import dev.argon.compiler.*
import dev.argon.compiler.definitions.*
import dev.argon.compiler.signature.Signature
import dev.argon.parser
import dev.argon.parser.{FunctionDeclarationStmt, IdentifierExpr}
import dev.argon.util.*
import zio.*

object SourceFunction {

  def make[TOwner]
  (ctx: Context)
  (imports: ctx.Comp[Imports[ctx.type]])
  (functionOwner: TOwner)
  (stmt: FunctionDeclarationStmt)
  : ctx.Comp[ArFuncC with HasContext[ctx.type] with HasOwner[TOwner]] =
    for
      funcId <- UniqueIdentifier.make
      sigCell <- MemoCell.make[ctx.Env, ctx.Error, Signature[ctx.ExprContext.WrapExpr, ctx.ExprContext.WrapExpr]]

    yield new ArFuncC {
      override val context: ctx.type = ctx
      import context.ExprContext.{WrapExpr, ArExpr, ExprConstructor}

      override val id: UniqueIdentifier = funcId

      override def signature: Comp[Signature[WrapExpr, WrapExpr]] =
        sigCell.get(???)

      override val owner: TOwner = functionOwner
    }

}
