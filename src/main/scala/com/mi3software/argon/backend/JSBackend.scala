package com.mi3software.argon.backend

import com.mi3software.argon.compiler.CompilationMessage
import com.mi3software.argon.compiler.js.JSContext
import com.mi3software.argon.parser.SourceAST
import scalaz.{NonEmptyList, \/}

object JSBackend extends Backend {

  override val id: String = "js"
  override val name: String = "JavaScript"

  override type TContext = JSContext

  override def createContext(sourceASTs: Vector[SourceAST]): JSContext = new JSContext

  override def getCompilationResult(context: JSContext): NonEmptyList[CompilationMessage] \/ CompilationResult =
    ???
}
