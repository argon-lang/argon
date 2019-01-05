package com.mi3software.argon.build

import com.mi3software.argon.compiler.{CompilationError, CompilationMessageNonFatal, CompilerInput}
import scalaz.Scalaz._
import scalaz._
import scalaz.effect.IO

trait Backend {
  val id: String
  val name: String

  def compile(input: CompilerInput): IO[CompilationResult]

}

object Backend {

  val allBackends = Vector(JSBackend)

  def find(id: String): Option[Backend] =
    allBackends.find { _.id === id }

}
