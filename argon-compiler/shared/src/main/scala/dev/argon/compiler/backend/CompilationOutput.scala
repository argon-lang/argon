package dev.argon.compiler.backend

import java.nio.charset.StandardCharsets

import cats._
import cats.data.NonEmptyList
import dev.argon.compiler.core.Context
import dev.argon.compiler.{CompilationError, ResourceAccess}
import dev.argon.stream._
import dev.argon.stream.builder.Source

trait CompilationOutput {
  val context: Context

  def write: context.Comp[Unit]
}

abstract class CompilationOutputText extends CompilationOutput {

  import context._

  implicit val resourceAccess: ResourceAccess[context.type]

  override def write: Comp[Unit] =
    resourceAccess.writeToResource(outputResource)(
      StringToByteStreamTransformation.convert(StandardCharsets.UTF_8)(textStream)
    )


  def outputResource: ResIndicator

  val textStream: Source[Comp, String, Unit]


}
