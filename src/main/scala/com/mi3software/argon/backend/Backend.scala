package com.mi3software.argon.backend

import com.mi3software.argon.compiler.{CompilationMessage, Context}
import com.mi3software.argon.parser.SourceAST
import scalaz.{NonEmptyList, \/}

trait Backend {
  val id: String
  val name: String

  type TContext <: Context

  def createContext(sourceASTs: Vector[SourceAST]): TContext
  def getCompilationResult(context: TContext): NonEmptyList[CompilationMessage] \/ CompilationResult
}
