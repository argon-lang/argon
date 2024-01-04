package dev.argon.plugin

import dev.argon.compiler.*
import dev.argon.compiler.definitions.HasImplementation
import dev.argon.compiler.tube.ArTubeC
import dev.argon.options.*
import dev.argon.io.*
import dev.argon.plugin.executor.TestExecutor
import dev.argon.util.*
import zio.*

import java.io.IOException

trait Plugin[R, E] {
  type Options
  type Output

  given optionCodec: OptionCodec[R, E, Options]
  given outputHandler: OutputHandler[R, E, Output]



  type ExternMethodImplementation
  type MethodReference

  type ExternFunctionImplementation
  type FunctionReference

  type ExternClassConstructorImplementation
  type ClassConstructorReference


  def emitTube
  (context: Context { type Env = R; type Error = E })
  (adapter: PluginContextAdapter.Aux[context.type, this.type])
  (tube: ArTubeC & HasContext[context.type] & HasImplementation[true])
  : context.Comp[Output]


  def loadExternMethod
  (options: Options)
  (id: String)
  : ZIO[R, E, Option[ExternMethodImplementation]]


  def loadExternFunction
  (options: Options)
  (id: String)
  : ZIO[R, E, Option[ExternFunctionImplementation]]

  def testExecutor: Option[TestExecutor[R, E, Options, Output]]

  def loadExternClassConstructor
  (options: Options)
  (id: String)
  : ZIO[R, E, Option[ExternClassConstructorImplementation]]

  def tubeLoaders[ContextOptions]: Map[String, TubeLoader[R, E, ContextOptions]]
}
