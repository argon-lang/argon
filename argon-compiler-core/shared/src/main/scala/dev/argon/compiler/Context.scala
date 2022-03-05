package dev.argon.compiler

import dev.argon.compiler.backend.*
import zio.*
import java.io.IOException
import dev.argon.parser.SyntaxError
import dev.argon.compiler.expr.CompleteExprContext
import dev.argon.compiler.tube.{TubeName, ArTubeC}

trait Context {
  val backend: BackendBase#BackendHandler
  type Comp[+A] = ZIO[CompEnv, CompError, A]

  object ExprContext extends CompleteExprContext {
    override val context: Context.this.type = Context.this
  }

  def getTube(tubeName: TubeName): Comp[ArTubeC with HasContext[this.type]]
}

object Context {
  type WithBackend[TBackend <: BackendBase#BackendHandler] = Context { val backend: TBackend }
}

type CompEnv = Random
type CompError = IOException | SyntaxError | DiagnosticError
