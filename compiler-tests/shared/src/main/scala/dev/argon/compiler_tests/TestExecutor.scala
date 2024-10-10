package dev.argon.compiler_tests

import dev.argon.plugin.{TubeEmitter, PluginError, PluginEnv, PluginCompatibleContext}
import zio.*
import scala.reflect.TypeTest
import esexpr.ESExpr
import dev.argon.compiler.ErrorLog
import dev.argon.compiler.TubeName

trait TestExecutor {
  val pluginId: String
  type Emitter[Ctx <: PluginCompatibleContext] <: TubeEmitter[Ctx]
  type CompiledProgram


  given emitterTypeTest[Ctx <: PluginCompatibleContext]: TypeTest[TubeEmitter[Ctx], Emitter[Ctx]]

  def options: ESExpr

  def outputOptions[E >: PluginError, Ctx <: PluginCompatibleContext { type Error = E }](emitter: Emitter[Ctx]): emitter.OutputOptions[E]

  def programState(context: PluginCompatibleContext)(emitter: Emitter[context.type])(output: emitter.Output[context.Error]): ZIO[context.Env, context.Error, CompiledProgram]

  def execute[E >: PluginError](libraries: Map[TubeName, CompiledProgram], program: CompiledProgram): ZIO[PluginEnv, E, TestExecutionResult]
}
