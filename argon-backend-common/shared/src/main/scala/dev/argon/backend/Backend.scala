package dev.argon.backend

import cats._
import dev.argon.compiler._
import dev.argon.compiler.loaders.ResourceIndicator
import zio.ZManaged

trait Backend {

  type TCompilationOutput <: CompilationOutput
  type BackendOptions[_[_], _]
  type BackendOptionsId[A] = BackendOptions[Id, A]

  val id: String
  val name: String

  def emptyBackendOptions[I]: BackendOptions[Option, I]
  def inferBackendOptions(compilerOptions: CompilerOptions[Id], options: BackendOptions[Option, String]): BackendOptionsId[String]
  def projectLoader[I]: ProjectLoader[BackendOptionsId[String], BackendOptionsId[I], I]
  def parseBackendOptions(table: toml.Value.Tbl): Either[toml.Parse.Error, BackendOptions[Option, String]]

  def compile
  (input: CompilerInput[ResourceIndicator, BackendOptions[Id, ResourceIndicator]])
  : ZManaged[ResourceAccess, ErrorList, TCompilationOutput]

}
