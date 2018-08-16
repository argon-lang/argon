package com.mi3software.argon.build

import java.io.File

import com.mi3software.argon.compiler.StandardCompilation._
import com.mi3software.argon.compiler.js.{JSAst, JSContext, JSEmitter}
import com.mi3software.argon.compiler.{CompilationError, CompilationMessageNonFatal, CompilerInput}
import com.mi3software.argon.util.{FileOperations, IOHelpers}
import scalaz.effect.IO
import scalaz.{NonEmptyList, \/}

object JSBackend extends Backend {

  override val id: String = "js"
  override val name: String = "JavaScript"

  override def compile(input: CompilerInput): IO[(Set[CompilationMessageNonFatal], NonEmptyList[CompilationError] \/ CompilationResult)] = {
    val context = new JSContext[StandardCompilationType]
    val emitter = new JSEmitter

    context.createModule(input).map {
      _
        .flatMap(emitter.emitModule(context))
        .map { jsModule =>
          new CompilationResult {
            override def writeToFile(outputFile: File): IO[Unit] =
              FileOperations.filePrintWriter(outputFile)(IOHelpers.impureFunction(JSAst.writeModule(jsModule)))
          }
        }
        .run.run.run(Set.empty)
    }
  }
}
