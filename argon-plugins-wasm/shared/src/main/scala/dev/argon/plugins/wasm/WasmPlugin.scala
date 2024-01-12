package dev.argon.plugins.wasm

import dev.argon.compiler.definitions.HasImplementation
import dev.argon.compiler.tube.ArTubeC
import dev.argon.compiler.{CompEnv, CompError, HasContext}
import dev.argon.options.{OptionDecoder, OutputHandler}
import dev.argon.plugin.executor.TestExecutor
import dev.argon.plugin.{Extern, PlatformPlugin, PluginAdapter, PluginContext, TubeLoader}
import zio.ZIO

type WasmEnv = CompEnv
type WasmError = CompError

final class WasmPlugin[R <: WasmEnv, E >: WasmError] extends PlatformPlugin[R, E] {
  override val pluginId: String = "wasm"

  override type Options = WasmOptions
  override type OutputOptions = WasmOutputOptions
  override type Output = WasmOutput

  override def optionDecoder: OptionDecoder[R, E, Options] =
    summon[OptionDecoder[R, E, Options]]

  override def outputOptionsDecoder: OptionDecoder[R, E, OutputOptions] =
    summon[OptionDecoder[R, E, OutputOptions]]

  override def outputHandler: OutputHandler[R, E, Output] =
    summon[OutputHandler[R, E, Output]]

  override val externMethod: WasmExtern.type = WasmExtern
  override val externFunction: WasmExtern.type = WasmExtern
  override val externClassConstructor: WasmExtern.type = WasmExtern

  override def loadExternMethod(options: Options)(id: String): ZIO[R, E, Option[externMethod.Implementation]] = ???

  override def loadExternFunction(options: Options)(id: String): ZIO[R, E, Option[externFunction.Implementation]] = ???

  override def loadExternClassConstructor(options: Options)(id: String): ZIO[R, E, Option[externClassConstructor.Implementation]] = ???

  override def emitTube
  (context: PluginContext[R, E, ?])
  (pluginAdapter: PluginAdapter[R, E, context.plugin.type, WasmPlugin.this.type])
  (tube: ArTubeC & HasContext[context.type] & HasImplementation[true])
  (options: OutputOptions)
  : context.Comp[Output] = ???

  override def testExecutor: Option[TestExecutor[R, E, Options, Output]] = None

  override def tubeLoaders: Map[String, TubeLoader[R, E, WasmPlugin.this.type]] = Map.empty
}
