package dev.argon.plugins.scheme

import dev.argon.plugin.scalaApi.TubeEmitter
import dev.argon.plugin.scalaApi.options.OutputHandler
import zio.*
import dev.argon.plugin.scalaApi.options.OptionDecoder
import dev.argon.plugin.scalaApi.options.OutputInfo
import dev.argon.plugin.scalaApi.vm.TubeDefinition

private[scheme] class SchemeEmitter[E <: Throwable] extends TubeEmitter[SchemeExterns, E, SchemeOutputOptions, SchemeOutput] {

  override def outputOptionsDecoder(): UIO[OptionDecoder[E, SchemeOutputOptions]] =
    ZIO.succeed(SchemeOutputOptions.optionDecoder)

  override def outputHandler(): UIO[OutputHandler[E, SchemeOutput]] =
    ZIO.succeed(new OutputHandler[E, SchemeOutput] {
      override def outputs(): UIO[Seq[OutputInfo[E, SchemeOutput]]] = ???
    })

  override def emitTube(tube: TubeDefinition[SchemeExterns, E], options: SchemeOutputOptions): IO[E, SchemeOutput] = ???
    
  
}
