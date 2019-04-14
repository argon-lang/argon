package com.mi3software.argon.build

import java.io.{File, PrintWriter, StringWriter}
import java.nio.charset.StandardCharsets

import com.mi3software.argon.compiler.js._
import com.mi3software.argon.compiler._
import com.mi3software.argon.util.FileOperations
import scalaz._
import Scalaz._
import com.mi3software.argon.build.project.ProjectLoader
import scalaz.Leibniz.===
import scalaz.zio.{IO, ZIO}
import toml.Codecs._
import shapeless.{ Id => _, _ }

object JSBackend extends Backend {

  override type TCompilationOutput[F[+_], I] = CompilationOutputText[F, I]
  override type BackendOptions[F[_], I] = JSBackendOptions[F, I]

  override val id: String = "js"
  override val name: String = "JavaScript"

  override def emptyBackendOptions[I]: JSBackendOptions[Option, I] = JSBackendOptions[Option, I](
    outputFile = None,
    extern = None,
  )
  override def inferBackendOptions(compilerOptions: CompilerOptions[Id], options: JSBackendOptions[Option, String]): BackendOptionsId[String] =
    JSBackendOptions[Id, String](
      outputFile = options.outputFile.getOrElse(compilerOptions.moduleName + ".js"),
      extern = options.extern.getOrElse(Map.empty)
    )

  override def projectLoader[F[_, _], I]: ProjectLoader[BackendOptionsId[String], BackendOptionsId[I], I] = {
    import shapeless._
    import ProjectLoader.Implicits._

    ProjectLoader[BackendOptionsId[String], BackendOptionsId[I], I]
  }

  override def parseBackendOptions(table: toml.Value.Tbl): Either[toml.Codec.Error, JSBackendOptions[Option, String]] =
    toml.Toml.parseAs[JSBackendOptions[Option, String]](table)


  override def compile[F[+ _], I: Show, A](input: CompilerInput[I, BackendOptions[Id, I]])(f: CompilationOutputText[F, I] => F[A])(implicit comp: Compilation[F], res: ResourceAccess[F, I]): F[A] = {
    val context = new JSContext[F, I](input)
    val emitter = new JSEmitter[F, context.type](context)

    context.createModule { module =>
      emitter.emitModule(module).flatMap { jsModule =>
        f(createOutput(input.backendOptions.outputFile)(jsModule))
      }
    }
  }

  private def createOutput[F[+_], I](outputRes: I)(jsModule: JSModule): CompilationOutputText[F, I] = new CompilationOutputText[F, I] {

    override def outputResource: I = outputRes

    override def writeText(writer: PrintWriter): Unit =
      JSAst.writeModule(jsModule)(writer)

  }
}
