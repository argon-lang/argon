package dev.argon.backend.js

import java.io.{File, PrintWriter, StringWriter}
import java.nio.charset.StandardCharsets

import dev.argon.compiler._
import cats._
import cats.instances._
import cats.data.NonEmptyList
import dev.argon.compiler.backend.{Backend, CompilationOutputText, ProjectLoader}
import dev.argon.stream.ArStream
import scalaz.zio.{IO, ZIO}
import toml.Codecs._
import shapeless.{Id => _, _}
import dev.argon.util.ExtraTomlCodecs._
import dev.argon.stream.builder.BuilderStream


object JSBackend extends Backend {

  override type TCompilationOutput[F[-_, +_, +_], R, I] = CompilationOutputText[F, R, I]
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
    import ProjectLoader.Implicits._

    ProjectLoader[BackendOptionsId[String], BackendOptionsId[I], I]
  }

  override def parseBackendOptions(table: toml.Value.Tbl): Either[toml.Codec.Error, JSBackendOptions[Option, String]] =
    toml.Toml.parseAs[JSBackendOptions[Option, String]](table)

  override def compile[F[-_, +_, +_], R, I: Show, A]
  (input: CompilerInput[I, JSBackendOptions[Id, I]])
  (f: CompilationOutputText[F, R, I] => F[R, NonEmptyList[CompilationError], A])
  (implicit compInstance: CompilationRE[F, R], res: ResourceAccess[F, R, I])
  : F[R, NonEmptyList[CompilationError], A] = {
    val context = new JSContext[F, R, I](input)
    val emitter = new JSEmitter[F, R, context.type](context, input.backendOptions.inject)

    context.createModule { module =>
      compInstance.flatMap(emitter.emitModule(module)) { jsModule =>
        f(createOutput(input.backendOptions.outputFile)(jsModule))
      }
    }
  }

  private def createOutput[F[-_, +_, +_], R, I](outputRes: I)(jsModule: JSModule)(implicit monadInstance: Monad[F[R, NonEmptyList[CompilationError], ?]]) : CompilationOutputText[F, R, I] = new CompilationOutputText[F, R, I] {

    override def outputResource: I = outputRes


    override def textStream: ArStream[F, R, NonEmptyList[CompilationError], String] =
      BuilderStream.toStream(
        JSAst.writeModule[BuilderStream[F[R, NonEmptyList[CompilationError], ?], String, ?]](jsModule)
      )

  }
}
