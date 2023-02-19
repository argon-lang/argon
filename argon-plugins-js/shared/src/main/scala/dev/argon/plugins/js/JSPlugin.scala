package dev.argon.plugins.js

import dev.argon.compiler.*
import dev.argon.compiler.definitions.HasImplementation
import dev.argon.compiler.tube.ArTubeC
import dev.argon.compiler.vtable.VTableBuilder
import dev.argon.options.*
import dev.argon.io.ResourceFactory
import dev.argon.plugin.*
import dev.argon.plugins.js.emit.TubeEmitter
import dev.argon.plugins.js.estree.FunctionDeclaration
import zio.*
import zio.stream.*

import java.nio.charset.CharacterCodingException

final class JSPlugin[R, E >: JSPluginError] extends Plugin[R, E] {
  override type Options = JSOptions[R, E]
  override type Output = JSOutput[R, E]

  override def optionCodec: OptionCodec[R, E, Options] =
    summon[OptionCodec[R, E, Options]]

  override def outputHandler: OutputHandler[R, E, Output] =
    summon[OutputHandler[R, E, Output]]


  override type ExternMethodImplementation = estree.FunctionDeclaration
  override type ExternFunctionImplementation = estree.FunctionDeclaration
  override type ExternClassConstructorImplementation = estree.FunctionDeclaration

  override def emitTube
  (ctx: Context { type Env = R; type Error = E })
  (adapter2: PluginContextAdapter.Aux[ctx.type, this.type])
  (t: ArTubeC & HasContext[ctx.type] & HasImplementation[true])
  : ctx.Comp[JSOutput[ctx.Env, ctx.Error]] =
    new TubeEmitter {
      override val context: ctx.type = ctx
      override val options: JSOptions[ctx.Env, ctx.Error] = adapter2.extractOptions(t.options)
      override val tube: ArTube & HasImplementation[true] = t
      override val plugin: JSPlugin.this.type = JSPlugin.this
      override val adapter: PluginContextAdapter.Aux[context.type, JSPlugin.this.type] = adapter2
    }.emitTube



  override def loadExternMethod
  (options: JSOptions[R, E])
  (id: String)
  : ZIO[R, E, Option[estree.FunctionDeclaration]] =
    ZStream.fromIterable(options.extern.getOrElse(Seq.empty))
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


  override def loadExternFunction
  (options: JSOptions[R, E])
  (id: String)
  : ZIO[R, E, Option[FunctionDeclaration]] =
    loadExternMethod(options)(id)


  override def loadExternClassConstructor
  (options: JSOptions[R, E])
  (id: String)
  : ZIO[R, E, Option[FunctionDeclaration]] =
    loadExternMethod(options)(id)

  override def tubeLoaders[ContextOptions]: Map[String, TubeLoader[R, E, ContextOptions]] = Map.empty
}
