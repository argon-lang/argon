package com.mi3software.argon.build

import com.mi3software.argon.compiler._
import scalaz._
import Scalaz._

trait Backend {

  type TCompilationOutput <: CompilationOutput

  val id: String
  val name: String

  def compile[F[+_], G[_]: Monad, I: Show](input: CompilerInput[I])(implicit comp: CompilationExec[F, G], res: ResourceAccess[F, I]): G[CompilationResult[TCompilationOutput]]

}

object Backend {

  val allBackends = Vector(JSBackend)

  def find(id: String): Option[Backend] =
    allBackends.find { _.id === id }

}
