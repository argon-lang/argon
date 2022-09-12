package dev.argon.compiler

import zio.*

import java.io.IOException
import dev.argon.parser.SyntaxError
import dev.argon.compiler.expr.CompleteExprContext
import dev.argon.compiler.tube.{ArTubeC, TubeName}
import dev.argon.compiler.vtable.VTableContext
import dev.argon.options.OptionCodec

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
  given optionsCodec: OptionCodec[Env, Error, Options]

  type ExternMethodImplementation
  type ExternFunctionImplementation
  type ExternClassConstructorImplementation


  def getExternMethodImplementation(options: Options, id: String): ZIO[Env, Option[Error], ExternMethodImplementation]
  def getExternFunctionImplementation(options: Options, id: String): ZIO[Env, Option[Error], ExternFunctionImplementation]
  def getExternClassConstructorImplementation(options: Options, id: String): ZIO[Env, Option[Error], ExternClassConstructorImplementation]

}

type CompEnv = Any
type CompError = DiagnosticError
