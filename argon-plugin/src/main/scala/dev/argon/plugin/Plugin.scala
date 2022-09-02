package dev.argon.plugin

import dev.argon.compiler.*
import dev.argon.compiler.definitions.HasImplementation
import dev.argon.compiler.tube.ArTubeC
import dev.argon.options.*
import dev.argon.io.*
import dev.argon.util.*
import zio.*

import java.io.IOException

trait Plugin[-R0, +E0] {
  type Options[-_, +_]
  type Output[-_, +_]

  given optionDecoder[E >: E0]: OptionDecoder[E, Options[Any, E]]
  given outputHandler[R <: R0, E >: E0]: OutputHandler[R, E, Output[R, E]]

  type ExternMethodImplementation
  type ExternFunctionImplementation
  type ExternClassConstructorImplementation


  def emitTube
  (context: Context { type Env <: R0; type Error >: E0 })
  (adapter: PluginContextAdapter.Aux[context.type, this.type])
  (options: Options[context.Env, context.Error])
  (tube: ArTubeC & HasContext[context.type] & HasImplementation[true])
  : context.Comp[Output[context.Env, context.Error]]


  def loadExternMethod[R <: R0, E >: E0]
  (options: Options[R, E])
  (id: String)
  : ZIO[R, E, Option[ExternMethodImplementation]]


  def loadExternFunction[R <: R0, E >: E0]
  (options: Options[R, E])
  (id: String)
  : ZIO[R, E, Option[ExternFunctionImplementation]]


  def loadExternClassConstructor[R <: R0, E >: E0]
  (options: Options[R, E])
  (id: String)
  : ZIO[R, E, Option[ExternClassConstructorImplementation]]


  def tubeLoaders: Map[String, TubeLoader[R0, E0]]
}
