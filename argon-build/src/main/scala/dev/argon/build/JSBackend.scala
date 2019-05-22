package dev.argon.build

import java.io.{File, PrintWriter, StringWriter}
import java.nio.charset.StandardCharsets

import dev.argon.compiler.js._
import dev.argon.compiler._
import dev.argon.util.FileOperations
import cats._
import cats.instances._
import cats.data.NonEmptyList
import dev.argon.build.project.ProjectLoader
import scalaz.zio.{IO, ZIO}
import toml.Codecs._
import shapeless.{Id => _, _}
import dev.argon.util.ExtraTomlCodecs._

object JSBackend extends Backend {

  override type TCompilationOutput[F[+_, +_], I] = CompilationOutputText[F, I]
  override type BackendOptions[F[_], I] = JSBackendOptions[F, I]

  override val id: String = "js"
  override val name: String = "JavaScript"

  override def emptyBackendOptions[I]: JSBackendOptions[Option, I] = JSBackendOptions[Option, I](
    outputFile = None,
    extern = None,
    inject = None,
  )
  override def inferBackendOptions(compilerOptions: CompilerOptions[Id], options: JSBackendOptions[Option, String]): BackendOptionsId[String] =
    JSBackendOptions[Id, String](
      outputFile = options.outputFile.getOrElse(compilerOptions.moduleName + ".js"),
      extern = options.extern.getOrElse(Map.empty),
      inject = options.inject.getOrElse(JSInjectCode[Option](before = None, after = None)) match {
        case JSInjectCode(beforeOpt, afterOpt) =>
          JSInjectCode[Id](
            before = beforeOpt.flatten,
            after = afterOpt.flatten
          )
      }
    )

  override def projectLoader[I]: ProjectLoader[BackendOptionsId[String], BackendOptionsId[I], I] = {
    import shapeless._
    import ProjectLoader.Implicits._

    ProjectLoader[BackendOptionsId[String], BackendOptionsId[I], I]
  }

  override def parseBackendOptions(table: toml.Value.Tbl): Either[toml.Codec.Error, JSBackendOptions[Option, String]] =
    toml.Toml.parseAs[JSBackendOptions[Option, String]](table)

  override def compile[F[+_, +_] : CompilationE, I: Show, A]
  (input: CompilerInput[I, JSBackendOptions[Id, I]])
  (f: CompilationOutputText[F, I] => F[NonEmptyList[CompilationError], A])
  (implicit res: ResourceAccess[F[NonEmptyList[CompilationError], ?], I])
  : F[NonEmptyList[CompilationError], A] = {
    val context = new JSContext[F, I](input)
    val emitter = new JSEmitter[F, context.type](context, input.backendOptions.inject)

    context.createModule { module =>
      implicitly[CompilationE[F]].flatMap(emitter.emitModule(module)) { jsModule =>
        f(createOutput(input.backendOptions.outputFile)(jsModule))
      }
    }
  }

  private def createOutput[F[+_, +_], I](outputRes: I)(jsModule: JSModule): CompilationOutputText[F, I] = new CompilationOutputText[F, I] {

    override def outputResource: I = outputRes

    override def writeText(resourceAccess: ResourceAccess[F[NonEmptyList[CompilationError], ?], I])(writer: resourceAccess.PrintWriter): F[NonEmptyList[CompilationError], Unit] = {

      val strWriter = new StringWriter()
      JSAst.writeModule(jsModule)(new PrintWriter(strWriter))

      resourceAccess.writeText(writer, strWriter.toString)
    }

  }
}
