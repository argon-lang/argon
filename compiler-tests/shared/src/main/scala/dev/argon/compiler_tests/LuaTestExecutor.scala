package dev.argon.compiler_tests

import dev.argon.util.{*, given}
import dev.argon.plugins.lua.LuaEmitter
import dev.argon.plugins.lua.LuaOutput
import dev.argon.plugin.{PluginEnv, PluginError, PluginCompatibleContext}
import zio.*
import dev.argon.plugins.lua.LuaOutputOptions
import dev.argon.esexpr.Dictionary
import dev.argon.plugin.TubeEmitter
import scala.reflect.TypeTest
import dev.argon.esexpr.ESExpr
import dev.argon.plugins.lua.AST
import dev.argon.compiler.TubeName
import scala.util.Using
import scala.collection.mutable
import zio.stream.ZSink
import scala.jdk.CollectionConverters.*

final class LuaTestExecutor extends TestExecutor with LuaTestExecutorPlatform {
  override val pluginId: String = "lua"

  override type Emitter[Ctx <: PluginCompatibleContext] = LuaEmitter[Ctx]

  override type CompiledProgram = AST.Chunk

  override def emitterTypeTest[Ctx <: PluginCompatibleContext]: TypeTest[TubeEmitter[Ctx], LuaEmitter[Ctx]] =
    summon
  
  override def options: ESExpr = ESExpr.Constructed("lua-options", Map(), Seq())

  override def outputOptions[E >: PluginError, Ctx <: PluginCompatibleContext { type Error = E }](emitter: LuaEmitter[Ctx]): emitter.OutputOptions[E] = LuaOutputOptions()

  override def programState(context: PluginCompatibleContext)(emitter: Emitter[context.type])(output: emitter.Output[context.Error]): ZIO[context.Env, context.Error, CompiledProgram] =
    output.chunk.luaChunk

  override def execute[E >: PluginError](compiledLibraries: Map[TubeName, CompiledProgram], program: CompiledProgram): ZIO[PluginEnv, E, TestExecutionResult] =
    for
      libFS <- ZIO.foreach(compiledLibraries) { (tubeName, compiledProgram) =>
        for
          code <- compiledProgram.toStream.run(ZSink.mkString)
        yield tubeName.encode.replace(".", "_").nn + ".lua" -> code
      }

      programOutput <- program.toStream.run(ZSink.mkString)
      fs = libFS ++ Map(
        "Test.lua" -> programOutput,
        "ArgonRuntime.lua" -> libraries("Argon.Core/lua/ArgonRuntime.lua"),
        "main.lua" -> mainLuaScript,
      )

      res <- executeLua(fs)
    yield res

  private val mainLuaScript: String =
    """
      local ArgonRuntime = require "ArgonRuntime"
      local m = require "Test"

      local f = m[""]["main"][ArgonRuntime:intern_signature {parameters = {{type = "tuple",elements = {},},},result = {type = "tuple",elements = {},},}]

      f()
    """

  
}
