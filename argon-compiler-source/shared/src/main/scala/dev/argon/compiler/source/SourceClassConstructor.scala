package dev.argon.compiler.source

import dev.argon.compiler.*
import dev.argon.compiler.definitions.*
import dev.argon.util.*
import dev.argon.compiler.signature.Signature
import dev.argon.parser
import dev.argon.parser.{ClassConstructorDeclarationStmt, IdentifierExpr}
import zio.*

object SourceClassConstructor {

  def make(ctx: Context)(exprConverter: ExpressionConverter with HasContext[ctx.type])
    (outerEnv: ctx.Comp[exprConverter.Env])
    (
      owningClass: ArClassC with HasContext[ctx.type],
      accessModifier: AccessModifier,
    )(stmt: ClassConstructorDeclarationStmt)
    : ctx.Comp[ClassConstructorC with HasContext[ctx.type]] = ???

}
