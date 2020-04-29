package dev.argon.backend

import shapeless.Id
import dev.argon.compiler._
import dev.argon.compiler.loaders.ResourceIndicator
import zio._

trait Backend {

  type BackendOptions[_[_], _]
  type BackendOptionsId[A] = BackendOptions[Id, A]

  type BackendOutputOptions[_[_], _]
  type BackendOutputOptionsId[A] = BackendOutputOptions[Id, A]
  type TCompilationOutput <: CompilationOutput[BackendOutputOptionsId]

  val id: String
  val name: String

  def emptyBackendOptions[I]: BackendOptions[Option, I]
  def inferBackendOptions[I](compilerOptions: CompilerOptions[Id], options: BackendOptions[Option, I]): BackendOptionsId[I]
  def backendOptionsProjectLoader[IOld, I]: ProjectLoader[BackendOptionsId[IOld], BackendOptionsId[I], IOld, I]
  def parseBackendOptions(table: toml.Value.Tbl): Either[toml.Parse.Error, BackendOptions[Option, String]]

  def emptyOutputOptions[I]: BackendOutputOptions[Option, I]
  def inferOutputOptions(compilerOptions: CompilerOptions[Id], options: BackendOutputOptions[Option, String]): BackendOutputOptionsId[String]
  def outputOptionsProjectLoader[IOld, I]: ProjectLoader[BackendOutputOptionsId[IOld], BackendOutputOptionsId[I], IOld, I]
  def parseOutputOptions(table: toml.Value.Tbl): Either[toml.Parse.Error, BackendOutputOptions[Option, String]]

  def compile[I <: ResourceIndicator: Tagged]
  (input: CompilerInput[I, BackendOptions[Id, I]])
  : ZManaged[ResourceReader[I], ErrorList, TCompilationOutput]

}
