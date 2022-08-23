package dev.argon.compiler

import zio.*

import java.io.IOException
import dev.argon.parser.SyntaxError
import dev.argon.compiler.expr.CompleteExprContext
import dev.argon.compiler.tube.{ArTubeC, TubeName}
import dev.argon.compiler.vtable.VTableContext

trait Context {
  type Env <: CompEnv
  type Error >: CompError
  type Comp[+A] = ZIO[Env, Error, A]

  object ExprContext extends CompleteExprContext {
    override val context: Context.this.type = Context.this
  }

  object VT extends VTableContext {
    override val context: Context.this.type = Context.this
  }

  type Options

  type ExternMethodImplementation
  type ExternFunctionImplementation
  type ExternClassConstructorImplementation

  def getTube(tubeName: TubeName): Comp[ArTubeC & HasContext[this.type]]


  def getExternMethodImplementation(options: Options, id: String): ZIO[Env, Option[Error], ExternMethodImplementation]
  def getExternFunctionImplementation(options: Options, id: String): ZIO[Env, Option[Error], ExternFunctionImplementation]
  def getExternClassConstructorImplementation(options: Options, id: String): ZIO[Env, Option[Error], ExternClassConstructorImplementation]

}

type CompEnv = Any
type CompError = DiagnosticError
