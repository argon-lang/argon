package com.mi3software.argon.build

import com.mi3software.argon.compiler._
import scalaz._
import Scalaz._

trait Backend {

  type TCompilationOutput[F[+_]] <: CompilationOutput[F]

  val id: String
  val name: String

  def compile[F[+_], I: Show](input: CompilerInput[I])(implicit comp: Compilation[F], res: ResourceAccess[F, I]): F[TCompilationOutput[F]]

}

object Backend {

  val allBackends = Vector(JSBackend)

  def find(id: String): Option[Backend] =
    allBackends.find { _.id === id }

}
