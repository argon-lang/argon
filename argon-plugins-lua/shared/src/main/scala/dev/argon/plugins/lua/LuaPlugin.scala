package dev.argon.plugins.lua

import cats.data.OptionT
import dev.argon.argonvm.{VMContext, VMContextBuilder}
import dev.argon.plugin.{PlatformPlugin, PluginContext, PluginEnv, PluginError, TubeEmitter, TubeLoader}
import dev.argon.options.{OptionDecoder, OutputHandler}
import dev.argon.compiler.*
import dev.argon.esexpr.ESExprCodec
import dev.argon.io.Resource
import zio.{Tag, ZEnvironment, ZIO}
import zio.interop.catz.core.given

import java.nio.charset.CharacterCodingException

final class LuaPlugin extends PlatformPlugin {
  override val pluginId: String = "lua"

  override type PlatformOptions[E >: PluginError] = LuaOptions

  override def optionDecoder[E >: PluginError]: OptionDecoder[E, LuaOptions] =
    summon[OptionDecoder[E, LuaOptions]]

  trait LuaExtern extends Extern.Tagged {
    override type Implementation = LuaExternImplementation
    override def implementationCodec: ESExprCodec[Implementation] = summon[ESExprCodec[Implementation]]
    override def implementationTag: Tag[LuaExternImplementation] = summon

    override type Reference = LuaReference
    override def referenceCodec: ESExprCodec[Reference] = summon
    override def referenceTag: Tag[Reference] = summon
  }

  override val externFunction: LuaExtern = new LuaExtern {
    override def loadExtern[E >: PluginError]
    (options: PlatformOptions[E])
    (id: String)
    : OptionT[[A] =>> ZIO[PluginEnv, E, A], externFunction.Implementation] =
      OptionT.fromOption(options.externs.dict.get(id))

    override def defineReference[E >: PluginError]
    (options: LuaOptions)
    (definitionInfo: DefinitionInfo)
    : ZIO[PluginEnv, E, externFunction.Reference] =
      definitionInfo match {
        case DefinitionInfo.Global(tubeName, modulePath, name, sig) =>
          val emit = new EmitBase {
            override val plugin: LuaPlugin = LuaPlugin.this
          }

          ZIO.succeed(LuaReference.Global(
            emit.getTubePath(tubeName),
            modulePath.encode,
            emit.getIdentifierKeyExprMemo(name),
            emit.getErasedSigKeyExprMemo(sig)
          ))
      }
  }

  type VMContextIncluding = VMContext {
    type Env <: PluginEnv
    type Error >: PluginError
    val implementations: {
      type ExternFunctionImplementation <: ZEnvironment[externFunction.Implementation]
      type FunctionReference <: ZEnvironment[externFunction.Reference]
    }
  }


  override def emitter[Ctx <: ContextIncluding]: Option[TubeEmitter[Ctx]] =
    Some(new TubeEmitter[Ctx] {
      override type OutputOptions[E >: PluginError] = LuaOutputOptions
      override type Output[E >: PluginError] = LuaOutput[E]


      override def outputOptionsDecoder[E >: PluginError]: OptionDecoder[E, OutputOptions[E]] =
        summon[OptionDecoder[E, OutputOptions[E]]]

      override def outputHandler[E >: PluginError]: OutputHandler[E, Output[E]] =
        summon[OutputHandler[E, Output[E]]]

      override def emitTube
      (ctx: Ctx)
      (tube: ArTubeC & HasContext[ctx.type])
      (options: LuaOutputOptions)
      : ctx.Comp[LuaOutput[ctx.Error]] =
        val builder = VMContextBuilder(ctx)

        for
          env <- ZIO.environment[ctx.Env]

        yield LuaOutput(
          chunk = new LuaChunkResource[ctx.Error] with LuaChunkResource.Impl[ctx.Error] with Resource.WithoutFileName {
            override def luaChunk: ZIO[Any, ctx.Error, AST.Chunk] =
              new TubeEmit {
                override val plugin: LuaPlugin.this.type = LuaPlugin.this
                override val context: builder.vmContext.type = builder.vmContext
                override val currentTube: VMTube = builder.createTube(tube)
              }.emitTube.provideEnvironment(env)
          }
        )
      end emitTube
    })

  override def tubeLoaders[Ctx <: ContextOnlyIncluding]: Map[String, TubeLoader[Ctx]] = Map.empty
}
