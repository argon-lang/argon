package dev.argon.build

import java.io.File

import dev.argon.compiler._
import scalaz._
import Scalaz._
import dev.argon.build.project.{ProjectFileHandler, ProjectLoader}

trait Backend {

  type TCompilationOutput[F[+_], I] <: CompilationOutput[F, I]
  type BackendOptions[_[_], _]
  type BackendOptionsId[A] = BackendOptions[Id, A]

  val id: String
  val name: String

  def emptyBackendOptions[I]: BackendOptions[Option, I]
  def inferBackendOptions(compilerOptions: CompilerOptions[Id], options: BackendOptions[Option, String]): BackendOptionsId[String]
  def projectLoader[F[_, _], I]: ProjectLoader[BackendOptionsId[String], BackendOptionsId[I], I]
  def parseBackendOptions(table: toml.Value.Tbl): Either[toml.Codec.Error, BackendOptions[Option, String]]

  def compile[F[+_], I: Show, A](input: CompilerInput[I, BackendOptions[Id, I]])(f: TCompilationOutput[F, I] => F[A])(implicit comp: Compilation[F], res: ResourceAccess[F, I]): F[A]

}

object Backend {

  val allBackends = Vector(ArModuleBackend, JSBackend)

  def find(id: String): Option[Backend] =
    allBackends.find { _.id === id }

}
