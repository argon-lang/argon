package dev.argon.compiler

import zio.*
import java.io.IOException
import dev.argon.parser.SyntaxError
import dev.argon.compiler.expr.CompleteExprContext
import dev.argon.compiler.tube.{TubeName, ArTubeC}

trait Context {
  type Env <: CompEnv
  type Error >: CompError
  type Comp[+A] = ZIO[Env, Error, A]

  object ExprContext extends CompleteExprContext {
    override val context: Context.this.type = Context.this
  }

  def getTube(tubeName: TubeName): Comp[ArTubeC with HasContext[this.type]]
}

type CompEnv = Any
type CompError = DiagnosticError
