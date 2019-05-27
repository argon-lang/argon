package dev.argon.compiler.backend

import cats._
import cats.data.NonEmptyList
import cats.implicits._
import dev.argon.compiler._

trait Backend {

  type TCompilationOutput[F[-_, +_, +_], R, I] <: CompilationOutput[F, R, I]
  type BackendOptions[_[_], _]
  type BackendOptionsId[A] = BackendOptions[Id, A]

  val id: String
  val name: String

  def emptyBackendOptions[I]: BackendOptions[Option, I]
  def inferBackendOptions(compilerOptions: CompilerOptions[Id], options: BackendOptions[Option, String]): BackendOptionsId[String]
  def projectLoader[I]: ProjectLoader[BackendOptionsId[String], BackendOptionsId[I], I]
  def parseBackendOptions(table: toml.Value.Tbl): Either[toml.Codec.Error, BackendOptions[Option, String]]

  def compile[F[-_, +_, +_], R, I: Show, A]
  (input: CompilerInput[I, BackendOptions[Id, I]])
  (f: TCompilationOutput[F, R, I] => F[R, NonEmptyList[CompilationError], A])
  (implicit compInstance: CompilationRE[F, R], res: ResourceAccess[F, R, I])
  : F[R, NonEmptyList[CompilationError], A]

}
