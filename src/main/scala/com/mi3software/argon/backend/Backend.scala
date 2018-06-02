package com.mi3software.argon.backend

import com.mi3software.argon.compiler.CompilationMessage
import com.mi3software.argon.parser.SourceAST
import scalaz._
import Scalaz._

trait Backend {
  val id: String
  val name: String

  def compile(sourceASTs: Vector[SourceAST]): NonEmptyList[CompilationMessage] \/ CompilationResult

}

object Backend {

  private val allBackends = Vector(JSBackend)

  def find(id: String): Option[Backend] =
    allBackends.find { _.id === id }

}
