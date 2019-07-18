package dev.argon.compiler.backend

import java.nio.charset.StandardCharsets

import cats._
import cats.data.NonEmptyList
import dev.argon.compiler.core.Context
import dev.argon.compiler.{CompilationError, ResourceAccess}
import dev.argon.stream._

trait CompilationOutput {
  val context: Context

  def write: context.Comp[Unit]
}

abstract class CompilationOutputText extends CompilationOutput {

  import context._

  implicit val resourceAccess: ResourceAccess[context.type]

  override def write: context.Comp[Unit] =
    resourceAccess.resourceSink(outputResource).use { sink =>
      textStream.foldLeft(StringToByteStreamTransformation(StandardCharsets.UTF_8).into(sink))
    }


  def outputResource: context.ResIndicator

  def textStream: ArStream[context.CompRE, context.Environment, NonEmptyList[CompilationError], String]


}
