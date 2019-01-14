package com.mi3software.argon.build

import java.io.{File, PrintWriter, StringWriter}
import java.nio.charset.StandardCharsets

import com.mi3software.argon.compiler.StandardCompilation._
import com.mi3software.argon.compiler.js.{JSAst, JSContext, JSEmitter}
import com.mi3software.argon.compiler.{CompilationError, CompilationMessageNonFatal, CompilerInput}
import com.mi3software.argon.util.{FileOperations, IOHelpers}
import scalaz.effect.IO
import scalaz.{NonEmptyList, \/}

object JSBackend extends Backend {

  override val id: String = "js"
  override val name: String = "JavaScript"

  override def compile(input: CompilerInput): IO[CompilationResult] = {
    val context = new JSContext[StandardCompilationType]
    val emitter = new JSEmitter

    context.createModule(input).map { module =>
      val (messages, result) = module
        .flatMap(emitter.emitModule(context))
        .map { jsModule =>
          new CompilationOutput {
            override def writeToFile(outputFile: File): IO[Unit] =
              FileOperations.filePrintWriter(outputFile)(IOHelpers.impureFunction(JSAst.writeModule(jsModule)))

            override def toByteArray: IO[Array[Byte]] = IO {
              val writer = new StringWriter()
              val printWriter = new PrintWriter(writer)
              JSAst.writeModule(jsModule)(printWriter)
              writer.toString.getBytes(StandardCharsets.UTF_8)
            }
          }
        }
        .run
        .run
        .run(Set.empty)

      CompilationResult(messages, result)
    }
  }
}
