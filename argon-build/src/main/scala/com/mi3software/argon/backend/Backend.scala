package com.mi3software.argon.backend

import com.mi3software.argon.compiler.{CompilationError, CompilationMessageNonFatal, CompilerInput}
import scalaz._
import Scalaz._
import scalaz.effect.IO

trait Backend {
  val id: String
  val name: String

  def compile(input: CompilerInput): IO[(Set[CompilationMessageNonFatal], NonEmptyList[CompilationError] \/ CompilationResult)]

}

object Backend {

  private val allBackends = Vector(JSBackend)

  def find(id: String): Option[Backend] =
    allBackends.find { _.id === id }

}
