package com.mi3software.argon.build

import java.io.{File, PrintWriter, StringWriter}
import java.nio.charset.StandardCharsets

import com.mi3software.argon.compiler.js.{JSAst, JSContext, JSEmitter, JSModule}
import com.mi3software.argon.compiler._
import com.mi3software.argon.util.{FileOperations, IOHelpers}
import scalaz._
import Scalaz._
import scalaz.Leibniz.===
import scalaz.zio.{IO, ZIO}

object JSBackend extends Backend {

  override type TCompilationOutput[F[+_]] = CompilationOutputText[F]

  override val id: String = "js"
  override val name: String = "JavaScript"


  override def compile[F[+_], I: Show](input: CompilerInput[I])(implicit comp: Compilation[F], res: ResourceAccess[F, I]): F[CompilationOutputText[F]] = {
    val context = new JSContext[F]
    val emitter = new JSEmitter[F, context.type](context)

    context.createModule(input)
      .flatMap(emitter.emitModule)
      .map(createOutput[F])
  }

  private def createOutput[F[+_]](jsModule: JSModule): CompilationOutputText[F] = new CompilationOutputText[F] {

    override def writeText(writer: PrintWriter): Unit =
      JSAst.writeModule(jsModule)(writer)

  }
}
