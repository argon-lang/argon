package com.mi3software.argon.backend

import java.io.File

import com.mi3software.argon.compiler.{CompilationMessage, CompilerInput}
import com.mi3software.argon.compiler.js.{JSAst, JSContext, JSEmitter}
import com.mi3software.argon.util.{FileOperations, IOHelpers}
import scalaz.effect.IO
import scalaz._
import Scalaz._

object JSBackend extends Backend {

  override val id: String = "js"
  override val name: String = "JavaScript"

  override def compile(input: CompilerInput): NonEmptyList[CompilationMessage] \/ CompilationResult = {
    val context = new JSContext
    val emitter = new JSEmitter
    val module = context.createModule(input)

    emitter.emitModule(context)(module).run match {
      case -\/(_) => ???
      case \/-((head +: tail, _)) =>
        -\/(NonEmptyList.nel(head, tail.toIList))

      case \/-((Vector(), jsModule)) =>
        \/-(new CompilationResult {
          override def writeToFile(outputFile: File): IO[Unit] =
            FileOperations.filePrintWriter(outputFile)(IOHelpers.impureFunction(JSAst.writeModule(jsModule)))
        })
    }


  }
}
