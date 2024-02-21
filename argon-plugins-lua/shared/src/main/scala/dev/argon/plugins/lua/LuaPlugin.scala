package dev.argon.plugins.lua

import dev.argon.compiler.definitions.HasImplementation
import dev.argon.compiler.tube.ArTubeC
import dev.argon.plugin.{PlatformPlugin, PluginAdapter, PluginContext, TubeLoader}
import dev.argon.options.{OptionDecoder, OutputHandler}
import dev.argon.compiler.{CompEnv, CompError, HasContext}
import dev.argon.io.Resource
import dev.argon.plugin.executor.TestExecutor
import zio.ZIO

import java.nio.charset.CharacterCodingException

type LuaEnv = CompEnv
type LuaError = CompError | CharacterCodingException

class LuaPlugin[R <: LuaEnv, E >: LuaError] extends PlatformPlugin[R, E] {
  override val pluginId: String = "lua"

  override type Options = LuaOptions
  override type OutputOptions = LuaOutputOptions
  override type Output = LuaOutput[R, E]

  override def optionDecoder: OptionDecoder[R, E, Options] =
    summon[OptionDecoder[R, E, Options]]

  override def outputOptionsDecoder: OptionDecoder[R, E, OutputOptions] =
    summon[OptionDecoder[R, E, OutputOptions]]

  override def outputHandler: OutputHandler[R, E, Output] =
    summon[OutputHandler[R, E, Output]]
    
  override val externMethod: LuaExtern.type = LuaExtern
  override val externFunction: LuaExtern.type = LuaExtern
  override val externClassConstructor: LuaExtern.type = LuaExtern

  override def loadExternMethod(options: Options)(id: String): ZIO[R, E, Option[externMethod.Implementation]] =
    ZIO.none

  override def loadExternFunction(options: Options)(id: String): ZIO[R, E, Option[externFunction.Implementation]] =
    ZIO.none

  override def loadExternClassConstructor(options: Options)(id: String): ZIO[R, E, Option[externClassConstructor.Implementation]] =
    ZIO.none

  override def emitTube
  (ctx: PluginContext[R, E, ?])
    (pluginAdapter0: PluginAdapter[R, E, ctx.plugin.type, LuaPlugin.this.type])
    (tube: ArTubeC & HasContext[ctx.type] & HasImplementation[true])
    (options: OutputOptions)
  : ctx.Comp[Output] =
    ZIO.succeed(LuaOutput(
      chunk = new LuaChunkResource[R, E] with LuaChunkResource.Impl[R, E] with Resource.WithoutFileName {
        override def luaChunk: ZIO[R, E, AST.Chunk] =
          new TubeEmitter[R, E] {
            override val context: ctx.type = ctx
            override val plugin: LuaPlugin.this.type = ???
            override val pluginAdapter: PluginAdapter[R, E, ctx.plugin.type, LuaPlugin.this.type] = pluginAdapter0
            override val currentTube: ArTube & HasImplementation[true] = tube
          }.emitTube
      }
    ))

  override def testExecutor: Option[TestExecutor[R, E, Options, Output]] = None

  override def tubeLoaders: Map[String, TubeLoader[R, E, LuaPlugin.this.type]] = Map.empty
}
