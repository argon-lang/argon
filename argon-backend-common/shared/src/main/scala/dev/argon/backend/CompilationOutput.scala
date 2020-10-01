package dev.argon.backend

import java.nio.charset.StandardCharsets

import cats._
import dev.argon.compiler.{Comp, CompilationError, RComp}
import dev.argon.compiler.core.Context
import dev.argon.compiler.loaders.ResourceIndicator
import dev.argon.stream._
import zio._
import zio.stream._
import zio.interop.catz.core._

trait CompilationOutput[OutputOptions[_]] {
  def write[I <: ResourceIndicator: Tag](options: OutputOptions[I]): RComp[ResourceWriter[I], Unit]
}

abstract class CompilationOutputText[OutputOptions[_]] extends CompilationOutput[OutputOptions] {

  override def write[I <: ResourceIndicator: Tag](options: OutputOptions[I]): RComp[ResourceWriter[I], Unit] =
    ZIO.accessM[ResourceWriter[I]](_.get.writeToResource(outputResource(options))(
      StringToByteStreamTransformation.convert(StandardCharsets.UTF_8)(textStream)
    ))

  def outputResource[I](options: OutputOptions[I]): I

  val textStream: Stream[CompilationError, String]


}
