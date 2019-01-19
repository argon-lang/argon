package com.mi3software.argon.build

import java.io.{File, PrintWriter, StringWriter}
import java.nio.charset.StandardCharsets

import com.mi3software.argon.compiler.js.{JSAst, JSContext, JSEmitter, JSModule}
import com.mi3software.argon.compiler._
import com.mi3software.argon.util.{FileOperations, IOHelpers}
import scalaz._
import Scalaz._

object JSBackend extends Backend {

  override type TCompilationOutput = CompilationOutputText

  override val id: String = "js"
  override val name: String = "JavaScript"


  override def compile[F[+_], G[_]: Monad, I: Show](input: CompilerInput[I])(implicit comp: CompilationExec[F, G], res: ResourceAccess[F, I]): G[CompilationResult[TCompilationOutput]] = {
    val context = new JSContext[F]
    val emitter = new JSEmitter[F, context.type](context)

    comp.getResult(
      context.createModule(input)
        .flatMap(emitter.emitModule)
        .map(createOutput)
    )
      .map { case (messages, result) =>
        CompilationResult(messages.toSet, result)
      }

  }

  private def createOutput(jsModule: JSModule): CompilationOutputText = new CompilationOutputText {

    override def writeText(writer: PrintWriter): Unit =
      JSAst.writeModule(jsModule)(writer)

  }
}
