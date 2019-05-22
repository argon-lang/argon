package dev.argon.build

import java.io.{OutputStream, PrintWriter}
import java.nio.charset.StandardCharsets

import dev.argon.compiler.{CompilationError, ResourceAccess}
import cats._
import cats.data.NonEmptyList
import cats.instances._

trait CompilationOutput[F[+_, +_], I] {
  def write(implicit resourceAccess: ResourceAccess[F[NonEmptyList[CompilationError], ?], I]): F[NonEmptyList[CompilationError], Unit]
}

trait CompilationOutputText[F[+_, +_], I] extends CompilationOutput[F, I] {

  override def write(implicit resourceAccess: ResourceAccess[F[NonEmptyList[CompilationError], ?], I]): F[NonEmptyList[CompilationError], Unit] =
    resourceAccess.createPrintWriter(outputResource)(writeText(resourceAccess)(_))

  def outputResource: I

  def writeText(resourceAccess: ResourceAccess[F[NonEmptyList[CompilationError], ?], I])(writer: resourceAccess.PrintWriter): F[NonEmptyList[CompilationError], Unit]

}
