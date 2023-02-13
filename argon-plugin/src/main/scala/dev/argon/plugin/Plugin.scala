package dev.argon.plugin

import dev.argon.compiler.*
import dev.argon.compiler.definitions.HasImplementation
import dev.argon.compiler.tube.ArTubeC
import dev.argon.options.*
import dev.argon.io.*
import dev.argon.util.*
import zio.*

import java.io.IOException

trait Plugin[+E0] {
  type Options[-_, +_]
  type Output[-_, +_]

  given optionCodec[R, E >: E0]: OptionCodec[R, E, Options[R, E]]
  given outputHandler[R, E >: E0]: OutputHandler[R, E, Output[R, E]]



  type ExternMethodImplementation
  type ExternFunctionImplementation
  type ExternClassConstructorImplementation


  def emitTube
  (context: Context { type Error >: E0 })
  (adapter: PluginContextAdapter.Aux[context.type, this.type])
  (tube: ArTubeC & HasContext[context.type] & HasImplementation[true])
  : context.Comp[Output[context.Env, context.Error]]


  def loadExternMethod[R, E >: E0]
  (options: Options[R, E])
  (id: String)
  : ZIO[R, E, Option[ExternMethodImplementation]]


  def loadExternFunction[R, E >: E0]
  (options: Options[R, E])
  (id: String)
  : ZIO[R, E, Option[ExternFunctionImplementation]]


  def loadExternClassConstructor[R, E >: E0]
  (options: Options[R, E])
  (id: String)
  : ZIO[R, E, Option[ExternClassConstructorImplementation]]


  def tubeLoaders: Map[String, TubeLoader[E0]]
}
