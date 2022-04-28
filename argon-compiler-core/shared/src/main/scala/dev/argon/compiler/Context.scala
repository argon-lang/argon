package dev.argon.compiler

import zio.*
import java.io.IOException
import dev.argon.parser.SyntaxError
import dev.argon.compiler.expr.CompleteExprContext
import dev.argon.compiler.tube.{TubeName, ArTubeC}

trait Context {
  type Comp[+A] = ZIO[CompEnv, CompError, A]

  object ExprContext extends CompleteExprContext {
    override val context: Context.this.type = Context.this
  }

  def getTube(tubeName: TubeName): Comp[ArTubeC with HasContext[this.type]]
}

type CompEnv = Any
type CompError = IOException | SyntaxError | DiagnosticError
