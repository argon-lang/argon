package dev.argon.compiler_tests

import dev.argon.plugin.{TubeEmitter, PluginError, PluginEnv, PluginCompatibleContextE}
import zio.*
import scala.reflect.TypeTest
import esexpr.ESExpr
import dev.argon.compiler.ErrorLog
import dev.argon.compiler.TubeName

trait TestExecutor[E >: PluginError] {
  val pluginId: String
  type Emitter[Ctx <: PluginCompatibleContextE[E]] <: TubeEmitter[E, Ctx]
  type CompiledProgram


  given emitterTypeTest[Ctx <: PluginCompatibleContextE[E]]: TypeTest[TubeEmitter[E, Ctx], Emitter[Ctx]]

  def options: ESExpr

  def outputOptions[Ctx <: PluginCompatibleContextE[E]](emitter: Emitter[Ctx]): emitter.OutputOptions

  def programState(context: PluginCompatibleContextE[E])(emitter: Emitter[context.type])(output: emitter.Output): ZIO[context.Env, context.Error, CompiledProgram]

  def execute(libraries: Map[TubeName, CompiledProgram], program: CompiledProgram): ZIO[PluginEnv, E, TestExecutionResult]
}
