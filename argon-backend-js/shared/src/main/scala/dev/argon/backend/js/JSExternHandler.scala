package dev.argon.backend.js

import java.nio.charset.StandardCharsets

import dev.argon.backend.ExternHandler
import dev.argon.compiler._
import zio.stream.ZStream
import zio._
import cats.implicits._
import cats.Id
import dev.argon.options.Options
import dev.argon.io.fileio.FileIO
import dev.argon.util.{MaybeBlocking, ValueCache}
import zio.interop.catz.core._

sealed trait JSExternHandler extends ExternHandler {
  override type ExternFunction = String
  override type ExternMethod = String
}

object JSExternHandler {
  def make(backendOptions: Options[Id, JSBackendOptionID]): URIO[FileIO with MaybeBlocking, JSExternHandler] = for {
    env <- ZIO.environment[FileIO]
    moduleExtractor <- JSModuleExtractorFactory.make
    externFunctionsCache <- ValueCache.make[CompilationError, Map[String, ResolvedExtern]]
  } yield new JSExternHandler {
    private def getExterns(source: DiagnosticSource): Comp[Map[String, ResolvedExtern]] =
      externFunctionsCache.get(
        backendOptions.get(JSBackendOptionID.Externs).files.foldMapM { file =>
          for {
            jsModule <- env.get.readAllText(file).catchAll(Compilation.unwrapThrowable)
            map <- moduleExtractor.exportedFunctions(jsModule).mapError { _ => DiagnosticError.InvalidExternFunction(source) }
          } yield map.view.mapValues[ResolvedExtern](ResolvedExtern.Function.apply).toMap
        }
      )

    override def loadExternFunction(source: DiagnosticSource, name: String): Comp[String] =
      getExterns(source).flatMap { externs =>
        externs.get(name) match {
          case Some(ResolvedExtern.Function(impl)) => IO.succeed(impl)
          case Some(ResolvedExtern.Ambiguous) => Compilation.forErrors(DiagnosticError.AmbiguousExtern(name, source))
          case None => Compilation.forErrors(DiagnosticError.UnknownExternImplementation(name, source))
        }
      }

    override def loadExternMethod(source: DiagnosticSource, name: String): Comp[String] = loadExternFunction(source, name)


    override def encodeExternalFunction(source: DiagnosticSource, extern: String)(platform: String): CompStream[Byte] =
      ZStream.unwrap(
        Compilation.require(platform === JSBackend.PlatformId)(DiagnosticError.ExternPlatformNotSupported(source, extern))
          .as(ZStream.fromChunk(Chunk.fromArray(extern.getBytes(StandardCharsets.UTF_8))))
      )

    override def encodeExternalMethod(source: DiagnosticSource, extern: String)(platform: String): CompStream[Byte] =
      encodeExternalFunction(source, extern)(platform)

  }
}
