package dev.argon.build

import dev.argon.compiler._
import dev.argon.parser.{SourceAST, SyntaxError, SyntaxErrorData}
import cats._
import cats.arrow.FunctionK
import cats.implicits._
import cats.data.{NonEmptyList, NonEmptyVector}
import dev.argon.backend.Backend
import dev.argon.compiler.loaders.{ResourceIndicator, ResourceReader}
import dev.argon.compiler.options.{CompilerInput, CompilerOptions}
import dev.argon.parser.impl.{ArgonSourceParser, ParseHandler}
import dev.argon.util.FileSpec
import dev.argon.stream._
import dev.argon.stream.builder.Source
import dev.argon.util.AnyExtensions._
import zio._
import zio.interop.catz.core._
import zio.stream._

object BuildProcess {

  def compile[I <: ResourceIndicator: Tag]
  (
    backend: Backend
  )(
    compilerOptions: CompilerOptions[Id, I],
    backendOptions: backend.BackendOptions[Id, I]
  )
  : ZManaged[ResourceReader[I], CompilationError, backend.TCompilationOutput] = {
    val input = CompilerInput(
      options = compilerOptions,
      backendOptions = backendOptions,
    )

    backend.compile(input).provideSomeLayer[ResourceReader[I]](ArgonSourceParser.live)
  }


}
