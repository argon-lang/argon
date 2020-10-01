package dev.argon.backend

import shapeless.Id
import dev.argon.compiler._
import dev.argon.compiler.loaders.{ResourceIndicator, ResourceReader, SourceParser}
import dev.argon.compiler.options.{CompilerInput, OptionsHandler}
import zio._

trait Backend {

  type BackendOptions[_[_], _]

  type BackendOutputOptions[_[_], _]
  type BackendOutputOptionsId[A] = BackendOutputOptions[Id, A]
  type TCompilationOutput <: CompilationOutput[BackendOutputOptionsId]

  val id: String
  val name: String

  val backendOptions: OptionsHandler[BackendOptions]
  val outputOptions: OptionsHandler[BackendOutputOptions]
  def testOutputOptions[I](dummyFile: I): BackendOutputOptions[Id, I]

  def compile[I <: ResourceIndicator: Tag]
  (input: CompilerInput[I, BackendOptions[Id, I]])
  : ZManaged[ResourceReader[I] with SourceParser, CompilationError, TCompilationOutput]

}
