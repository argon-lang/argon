package dev.argon.build

import java.io.{OutputStream, PrintWriter}
import java.nio.charset.StandardCharsets

import dev.argon.compiler.{CompilationError, ResourceAccess}
import cats._
import cats.data.NonEmptyList
import cats.instances._
import dev.argon.util.stream.{ArStream, StringToByteStreamTransformation}

trait CompilationOutput[F[-_, +_, +_], I] {
  def write(implicit resourceAccess: ResourceAccess[F, I]): F[Any, NonEmptyList[CompilationError], Unit]
}

abstract class CompilationOutputText[F[-_, +_, +_], I](implicit monadInstance: Monad[F[Any, NonEmptyList[CompilationError], ?]]) extends CompilationOutput[F, I] {

  override def write(implicit resourceAccess: ResourceAccess[F, I]): F[Any, NonEmptyList[CompilationError], Unit] =
    resourceAccess.resourceSink(outputResource).use { sink =>
      textStream.foldLeft(StringToByteStreamTransformation(StandardCharsets.UTF_8).into(sink))
    }

  def outputResource: I

  def textStream: ArStream[F, Any, NonEmptyList[CompilationError], String]


}
