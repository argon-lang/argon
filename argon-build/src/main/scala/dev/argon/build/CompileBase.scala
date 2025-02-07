package dev.argon.build

import dev.argon.io.*
import dev.argon.compiler.*
import esexpr.{ESExpr, ESExprCodec}
import zio.*
import zio.stm.*

import java.io.IOException
import dev.argon.tube.resource.TubeResourceContext

trait CompileBase extends UsingContext {
  override val context: CContext

  type BuildOutput


  def compile(): ZIO[Scope & context.Env, context.Error, BuildOutput] =
    compileImpl()
      .ensuring { ZIO.serviceWithZIO[LogReporter](_.reportLogs) }
      .tap { _ => ZIO.serviceWithZIO[LogReporter](_.failOnErrors) }

  protected def compileImpl(): ZIO[Scope & context.Env, context.Error, BuildOutput]

}
