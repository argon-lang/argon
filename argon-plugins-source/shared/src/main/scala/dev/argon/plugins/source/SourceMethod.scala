package dev.argon.plugins.source

import dev.argon.compiler.*
import dev.argon.compiler.definitions.*
import dev.argon.util.*
import dev.argon.compiler.signature.Signature
import dev.argon.parser
import dev.argon.parser.{MethodDeclarationStmt, IdentifierExpr}
import zio.*

object SourceMethod {

  def make[TOwner](ctx: Context)(exprConverter: ExpressionConverter with HasContext[ctx.type])
    (outerEnv: ctx.Comp[exprConverter.Env])(methodOwner: TOwner)(stmt: MethodDeclarationStmt)
    : ctx.Comp[ArMethodC with HasContext[ctx.type] with HasOwner[TOwner]] = ???

}
