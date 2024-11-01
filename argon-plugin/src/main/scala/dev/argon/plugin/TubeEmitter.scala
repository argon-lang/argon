package dev.argon.plugin

import dev.argon.compiler.*
import dev.argon.options.{OptionDecoder, OutputHandler}
import zio.*
import scala.reflect.TypeTest

trait TubeEmitter[E >: PluginError, Ctx <: PluginCompatibleContextE[E]] {
  type OutputOptions
  type Output

  given outputOptionsDecoder: OptionDecoder[OutputOptions]
  given outputHandler: OutputHandler[E, Output]

  def emitTube
  (context: Ctx)
  (tube: ArTubeC & HasContext[context.type])
  (options: OutputOptions)
  : context.Comp[Output]
}

trait CompoundTubeEmitter[E >: PluginError, Ctx <: PluginCompatibleContextE[E]] extends TubeEmitter[E, Ctx] {
  def extract[Emitter <: TubeEmitter[E, Ctx]](using TypeTest[TubeEmitter[E, Ctx], Emitter]): Option[Emitter]
}


sealed trait TubeEmitterSet[E >: PluginError, Ctx <: PluginCompatibleContextE[E]] {
  type OutputOptions
  type Output

  given outputOptionsDecoder: PluginSetUtil.PartialOptionDecoder[OutputOptions]

  given outputHandler: OutputHandler[E, Output]

  def extract[Emitter <: TubeEmitter[E, Ctx]](using TypeTest[TubeEmitter[E, Ctx], Emitter]): Option[Emitter]

  def emitTube
  (context: Ctx)
  (tube: ArTubeC & HasContext[context.type])
  (options: OutputOptions)
  : context.Comp[Output]

  def toEmitter: CompoundTubeEmitter[E, Ctx] =
    new CompoundTubeEmitter[E, Ctx] {
      override type OutputOptions = TubeEmitterSet.this.OutputOptions
      override type Output = TubeEmitterSet.this.Output

      override def outputOptionsDecoder: OptionDecoder[OutputOptions] =
        TubeEmitterSet.this.outputOptionsDecoder.toDecoder


      override def outputHandler: OutputHandler[E, TubeEmitterSet.this.Output] =
        TubeEmitterSet.this.outputHandler

      override def emitTube
      (context: Ctx)
      (tube: ArTubeC & HasContext[context.type])
      (options: OutputOptions)
      : context.Comp[Output] =
        TubeEmitterSet.this.emitTube(context)(tube)(options)

      def extract[Emitter <: TubeEmitter[E, Ctx]](using TypeTest[TubeEmitter[E, Ctx], Emitter]): Option[Emitter] =
        TubeEmitterSet.this.extract[Emitter]
    }
}

object TubeEmitterSet {

  final class Empty[E >: PluginError, Ctx <: PluginCompatibleContextE[E]] extends TubeEmitterSet[E, Ctx] {
    type OutputOptions = Unit
    type Output = Unit

    given outputOptionsDecoder: PluginSetUtil.PartialOptionDecoder[OutputOptions] =
      PluginSetUtil.PartialOptionDecoderEmpty()

    given outputHandler: OutputHandler[E, Output] =
      PluginSetUtil.OutputHandlerEmpty()

    def emitTube
    (context: Ctx)
      (tube: ArTubeC & HasContext[context.type])
      (options: OutputOptions)
    : context.Comp[Output] =
      ZIO.unit

    def extract[Emitter <: TubeEmitter[E, Ctx]](using TypeTest[TubeEmitter[E, Ctx], Emitter]): Option[Emitter] =
      None
  }

  sealed trait Singleton[E >: PluginError, Ctx <: PluginCompatibleContextE[E], TE <: TubeEmitter[E, Ctx]](pluginId: String) extends TubeEmitterSet[E, Ctx] {
    val tubeEmitter: TE

    type OutputOptions = tubeEmitter.OutputOptions
    type Output = tubeEmitter.Output

    given outputOptionsDecoder: PluginSetUtil.PartialOptionDecoder[OutputOptions] =
      PluginSetUtil.PartialOptionDecoderSingleton(pluginId)

    given outputHandler: OutputHandler[E, Output] =
      PluginSetUtil.OutputHandlerSingleton[E, Output](pluginId, tubeEmitter.outputHandler)

    def emitTube
    (context: Ctx)
      (tube: ArTubeC & HasContext[context.type])
      (options: OutputOptions)
    : context.Comp[Output] =
      tubeEmitter.emitTube(context)(tube)(options)

    def extract[Emitter <: TubeEmitter[E, Ctx]](using TypeTest[TubeEmitter[E, Ctx], Emitter]): Option[Emitter] =
      tubeEmitter match {
        case emitter: Emitter => Some(emitter)
        case _ => None
      }
  }

  object Singleton {
    def apply[E >: PluginError, Ctx <: PluginCompatibleContextE[E]](emitter: TubeEmitter[E, Ctx], pluginId: String): Singleton[E, Ctx, emitter.type] =
      new Singleton[E, Ctx, emitter.type](pluginId) {
        override val tubeEmitter: emitter.type = emitter
      }
  }

  sealed trait Union[E >: PluginError, Ctx <: PluginCompatibleContextE[E], TEA <: TubeEmitterSet[E, Ctx], TEB <: TubeEmitterSet[E, Ctx]] extends TubeEmitterSet[E, Ctx] {
    val aSet: TEA
    val bSet: TEB

    type OutputOptions = (aSet.OutputOptions, bSet.OutputOptions)
    type Output = (aSet.Output, bSet.Output)

    given outputOptionsDecoder: PluginSetUtil.PartialOptionDecoder[OutputOptions] =
      PluginSetUtil.PartialOptionDecoderUnion[aSet.OutputOptions, bSet.OutputOptions]

    given outputHandler: OutputHandler[E, Output] =
      PluginSetUtil.OutputHandlerUnion[E, aSet.Output, bSet.Output]()

    def emitTube
    (context: Ctx)
    (tube: ArTubeC & HasContext[context.type])
    (options: OutputOptions)
    : context.Comp[Output] =
      for
        aOut <- aSet.emitTube(context)(tube)(options._1)
        bOut <- bSet.emitTube(context)(tube)(options._2)
      yield (aOut, bOut)

    def extract[Emitter <: TubeEmitter[E, Ctx]](using TypeTest[TubeEmitter[E, Ctx], Emitter]): Option[Emitter] =
      aSet.extract[Emitter].orElse(bSet.extract[Emitter])
  }

  object Union {
    def apply[E >: PluginError, Ctx <: PluginCompatibleContextE[E]](a: TubeEmitterSet[E, Ctx], b: TubeEmitterSet[E, Ctx]): Union[E, Ctx, a.type, b.type] =
      new Union[E, Ctx, a.type, b.type] {
        override val aSet: a.type = a
        override val bSet: b.type = b
      }
  }
}
