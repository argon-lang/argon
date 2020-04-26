package dev.argon.backend

import java.nio.charset.StandardCharsets

import cats._
import dev.argon.compiler.{Comp, RComp}
import dev.argon.compiler.core.Context
import dev.argon.compiler.loaders.ResourceIndicator
import dev.argon.stream._
import dev.argon.stream.builder.Source
import zio.ZIO
import zio.interop.catz._

trait CompilationOutput {
  def write: RComp[ResourceAccess, Unit]
}

abstract class CompilationOutputText extends CompilationOutput {

  override def write: RComp[ResourceAccess, Unit] =
    ZIO.accessM[ResourceAccess](_.get.writeToResource(outputResource)(
      StringToByteStreamTransformation.convert(StandardCharsets.UTF_8)(textStream)
    ))

  def outputResource: ResourceIndicator

  val textStream: Source[Comp, String, Unit]


}
