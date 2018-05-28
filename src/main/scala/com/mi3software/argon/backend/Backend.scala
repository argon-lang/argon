package com.mi3software.argon.backend

import com.mi3software.argon.compiler.CompilationMessage
import com.mi3software.argon.parser.SourceAST
import scalaz.{NonEmptyList, \/}

trait Backend {
  val id: String
  val name: String

  def compile(sourceASTs: Vector[SourceAST]): NonEmptyList[CompilationMessage] \/ CompilationResult

}
