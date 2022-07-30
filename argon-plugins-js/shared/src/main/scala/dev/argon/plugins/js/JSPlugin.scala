package dev.argon.plugins.js

import dev.argon.compiler.*
import dev.argon.compiler.definitions.HasDeclaration
import dev.argon.compiler.tube.ArTubeC
import dev.argon.compiler.vtable.VTableBuilder
import dev.argon.options.*
import dev.argon.io.ResourceFactory
import dev.argon.plugin.*
import dev.argon.plugins.js.emit.TubeEmitter
import zio.*
import zio.stream.*

import java.nio.charset.CharacterCodingException

object JSPlugin extends Plugin[Any, JSPluginError] {
  override type Options[-R, +E] = JSOptions[R, E]
  override type Output[-R, +E] = JSOutput[R, E]

  override def optionDecoder[E >: JSPluginError]: OptionDecoder[E, Options[Any, E]] =
    summon[OptionDecoder[E, Options[Any, E]]]

  override def outputHandler[R, E >: JSPluginError]: OutputHandler[R, E, Output[R, E]] =
    summon[OutputHandler[R, E, Output[R, E]]]


  override type ExternalMethodImplementation = estree.FunctionDeclaration



  override def emitTube
  (ctx: Context { type Error >: JSPluginError })
  (adapter2: PluginContextAdapter.Aux[ctx.type, this.type])
  (opt: JSOptions[ctx.Env, ctx.Error])
  (t: ArTubeC with HasContext[ctx.type] with HasDeclaration[true])
  : ctx.Comp[JSOutput[ctx.Env, ctx.Error]] =
    for
      vtb <- VTableBuilder(ctx)
      output <- new TubeEmitter {
        override val context: ctx.type = ctx
        override val options: JSOptions[ctx.Env, ctx.Error] = opt
        override val tube: ArTube with HasDeclaration[true] = t
        override protected val vtableBuilder: VTableBuilder[context.type] = vtb
        override val adapter: PluginContextAdapter.Aux[context.type, JSPlugin.type] = adapter2
      }.emitTube
    yield output



  override def loadExternMethod[R <: Any, E >: JSPluginError]
  (options: JSOptions[R, E])
  (id: String)
  : ZIO[R, E, Option[estree.FunctionDeclaration]] =
    ZStream.fromIterable(options.extern)
      .mapZIO { _.asModule }
      .flatMap { module => ZStream.fromIterable(module.body) }
      .collect {
        case exportDecl: estree.ExportNamedDeclaration =>
          exportDecl.declaration.toOption
      }
      .collect {
        case Some(decl: estree.FunctionDeclaration) => decl
      }
      .filter { decl =>
        decl.id.toOption.exists { declId => declId.name == id }
      }
      .runHead


  override def tubeLoaders: Map[String, TubeLoader[Any, JSPluginError]] = Map.empty
  override def buildOutputExecutor: Option[BuildOutputExecutor[JSOutput]] = ???
}
