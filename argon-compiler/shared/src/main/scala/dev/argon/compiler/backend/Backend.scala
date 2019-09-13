package dev.argon.compiler.backend

import cats._
import cats.data.NonEmptyList
import cats.implicits._
import dev.argon.compiler._
import dev.argon.compiler.backend.Backend.ContextWithComp
import dev.argon.compiler.core.Context

trait Backend {

  type TCompilationOutput <: CompilationOutput
  type BackendOptions[_[_], _]
  type BackendOptionsId[A] = BackendOptions[Id, A]

  val id: String
  val name: String

  def emptyBackendOptions[I]: BackendOptions[Option, I]
  def inferBackendOptions(compilerOptions: CompilerOptions[Id], options: BackendOptions[Option, String]): BackendOptionsId[String]
  def projectLoader[I]: ProjectLoader[BackendOptionsId[String], BackendOptionsId[I], I]
  def parseBackendOptions(table: toml.Value.Tbl): Either[toml.Codec.Error, BackendOptions[Option, String]]

  def compile[F[+_], I: Show, A]
  (input: CompilerInput[I, BackendOptions[Id, I]])
  (f: TCompilationOutput { val context: ContextWithComp[F, I] } => F[A])
  (implicit compInstance: Compilation[F], resFactory: ResourceAccessFactory[ContextWithComp[F, I]])
  : F[A]

}

object Backend {
  type ContextWithComp[F[+_], I] = Context {
    type Comp[+A] = F[A]
    type ResIndicator = I
  }
}
