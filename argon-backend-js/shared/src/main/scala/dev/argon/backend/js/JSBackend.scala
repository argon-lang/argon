package dev.argon.backend.js

import dev.argon.compiler._
import cats._
import cats.instances._
import cats.data.NonEmptyList
import dev.argon.compiler.backend.Backend.ContextWithComp
import dev.argon.compiler.backend.{Backend, CompilationOutputText, ProjectLoader}
import dev.argon.compiler.core.Context
import dev.argon.stream.builder.Source
import zio.{IO, ZIO}
import toml.Codecs._
import shapeless.{Id => _, _}
import dev.argon.util.ExtraTomlCodecs._


object JSBackend extends Backend {

  override type TCompilationOutput = CompilationOutputText
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

  override def parseBackendOptions(table: toml.Value.Tbl): Either[toml.Parse.Error, JSBackendOptions[Option, String]] =
    toml.Toml.parseAs[JSBackendOptions[Option, String]](table)


  override def compile[F[+_], I: Show, A]
  (input: CompilerInput[I, JSBackendOptions[Id, I]])
  (f: CompilationOutputText { val context: ContextWithComp[F, I] } => F[A])
  (implicit compInstance: Compilation[F], resFactory: ResourceAccessFactory[ContextWithComp[F, I]])
  : F[A] = {
    val context = new JSContext[F, I](input)
    val emitter = new JSEmitter[F, context.type](context, input.backendOptions.inject)
    implicit val res = resFactory.create(context)

    context.createModule { module =>
      compInstance.flatMap(emitter.emitModule(module)) { jsModule =>
        f(createOutput(context)(input.backendOptions.outputFile)(jsModule))
      }
    }
  }

  private def createOutput(context2: Context)(outputRes: context2.ResIndicator)(jsModule: JSModule)(implicit resAccess: ResourceAccess[context2.type]): CompilationOutputText { val context: context2.type } = new CompilationOutputText {

    override val context: context2.type = context2
    override implicit val resourceAccess: ResourceAccess[context.type] = resAccess

    import context._

    override def outputResource: ResIndicator = outputRes

    override val textStream: Source[Comp, String, Unit] =
      JSAst.writeModule(jsModule)

  }

}
