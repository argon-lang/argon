package dev.argon.build

import java.io.{OutputStream, PrintWriter}
import java.nio.charset.StandardCharsets

import dev.argon.compiler.{CompilationError, ResourceAccess}
import cats._
import cats.data.NonEmptyList
import cats.instances._

trait CompilationOutput[F[-_, +_, +_], I] {
  def write(implicit resourceAccess: ResourceAccess[F, I]): F[Any, NonEmptyList[CompilationError], Unit]
}

trait CompilationOutputText[F[-_, +_, +_], I] extends CompilationOutput[F, I] {

  override def write(implicit resourceAccess: ResourceAccess[F, I]): F[Any, NonEmptyList[CompilationError], Unit] =
    resourceAccess.createPrintWriter(outputResource)(writeText(resourceAccess)(_))

  def outputResource: I

  def writeText(resourceAccess: ResourceAccess[F, I])(writer: resourceAccess.PrintWriter): F[Any, NonEmptyList[CompilationError], Unit]

}
