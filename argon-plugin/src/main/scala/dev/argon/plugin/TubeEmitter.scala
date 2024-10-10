package dev.argon.plugin

import dev.argon.compiler.*
import dev.argon.options.{OptionDecoder, OutputHandler}
import zio.*
import scala.reflect.TypeTest

trait TubeEmitter[Ctx <: PluginCompatibleContext] {
  type OutputOptions[E >: PluginError]
  type Output[E >: PluginError]

  given outputOptionsDecoder[E >: PluginError]: OptionDecoder[OutputOptions[E]]
  given outputHandler[E >: PluginError]: OutputHandler[E, Output[E]]

  def emitTube
  (context: Ctx)
  (tube: ArTubeC & HasContext[context.type])
  (options: OutputOptions[context.Error])
  : context.Comp[Output[context.Error]]
}

trait CompoundTubeEmitter[Ctx <: PluginCompatibleContext] extends TubeEmitter[Ctx] {
  def extract[Emitter <: TubeEmitter[Ctx]](using TypeTest[TubeEmitter[Ctx], Emitter]): Option[Emitter]
}


sealed trait TubeEmitterSet[Ctx <: PluginCompatibleContext] {
  type OutputOptions[E >: PluginError]
  type Output[E >: PluginError]

  given outputOptionsDecoder[E >: PluginError]: PluginSetUtil.PartialOptionDecoder[OutputOptions[E]]

  given outputHandler[E >: PluginError]: OutputHandler[E, Output[E]]

  def extract[Emitter <: TubeEmitter[Ctx]](using TypeTest[TubeEmitter[Ctx], Emitter]): Option[Emitter]

  def emitTube
  (context: Ctx)
  (tube: ArTubeC & HasContext[context.type])
  (options: OutputOptions[context.Error])
  : context.Comp[Output[context.Error]]

  def toEmitter: CompoundTubeEmitter[Ctx] =
    new CompoundTubeEmitter[Ctx] {
      override type OutputOptions[E >: PluginError] = TubeEmitterSet.this.OutputOptions[E]
      override type Output[E >: PluginError] = TubeEmitterSet.this.Output[E]

      override def outputOptionsDecoder[E >: PluginError]: OptionDecoder[OutputOptions[E]] =
        TubeEmitterSet.this.outputOptionsDecoder[E].toDecoder


      override def outputHandler[E >: PluginError]: OutputHandler[E, TubeEmitterSet.this.Output[E]] =
        TubeEmitterSet.this.outputHandler

      override def emitTube
      (context: Ctx)
      (tube: ArTubeC & HasContext[context.type])
      (options: OutputOptions[context.Error])
      : context.Comp[Output[context.Error]] =
        TubeEmitterSet.this.emitTube(context)(tube)(options)

      def extract[Emitter <: TubeEmitter[Ctx]](using TypeTest[TubeEmitter[Ctx], Emitter]): Option[Emitter] =
        TubeEmitterSet.this.extract[Emitter]
    }
}

object TubeEmitterSet {

  final class Empty[Ctx <: PluginCompatibleContext] extends TubeEmitterSet[Ctx] {
    type OutputOptions[E >: PluginError] = Unit
    type Output[E >: PluginError] = Unit

    given outputOptionsDecoder[E >: PluginError]: PluginSetUtil.PartialOptionDecoder[OutputOptions[E]] =
      PluginSetUtil.PartialOptionDecoderEmpty()

    given outputHandler[E >: PluginError]: OutputHandler[E, Output[E]] =
      PluginSetUtil.OutputHandlerEmpty()

    def emitTube
    (context: Ctx)
      (tube: ArTubeC & HasContext[context.type])
      (options: OutputOptions[context.Error])
    : context.Comp[Output[context.Error]] =
      ZIO.unit

    def extract[Emitter <: TubeEmitter[Ctx]](using TypeTest[TubeEmitter[Ctx], Emitter]): Option[Emitter] =
      None
  }

  sealed trait Singleton[Ctx <: PluginCompatibleContext, TE <: TubeEmitter[Ctx]](pluginId: String) extends TubeEmitterSet[Ctx] {
    val tubeEmitter: TE

    type OutputOptions[E >: PluginError] = tubeEmitter.OutputOptions[E]
    type Output[E >: PluginError] = tubeEmitter.Output[E]

    given outputOptionsDecoder[E >: PluginError]: PluginSetUtil.PartialOptionDecoder[OutputOptions[E]] =
      PluginSetUtil.PartialOptionDecoderSingleton(pluginId)

    given outputHandler[E >: PluginError]: OutputHandler[E, Output[E]] =
      PluginSetUtil.OutputHandlerSingleton[E, Output[E]](pluginId, tubeEmitter.outputHandler)

    def emitTube
    (context: Ctx)
      (tube: ArTubeC & HasContext[context.type])
      (options: OutputOptions[context.Error])
    : context.Comp[Output[context.Error]] =
      tubeEmitter.emitTube(context)(tube)(options)

    def extract[Emitter <: TubeEmitter[Ctx]](using TypeTest[TubeEmitter[Ctx], Emitter]): Option[Emitter] =
      tubeEmitter match {
        case emitter: Emitter => Some(emitter)
        case _ => None
      }
  }

  object Singleton {
    def apply[Ctx <: PluginCompatibleContext](emitter: TubeEmitter[Ctx], pluginId: String): Singleton[Ctx, emitter.type] =
      new Singleton[Ctx, emitter.type](pluginId) {
        override val tubeEmitter: emitter.type = emitter
      }
  }

  sealed trait Union[Ctx <: PluginCompatibleContext, TEA <: TubeEmitterSet[Ctx], TEB <: TubeEmitterSet[Ctx]] extends TubeEmitterSet[Ctx] {
    val aSet: TEA
    val bSet: TEB

    type OutputOptions[E >: PluginError] = (aSet.OutputOptions[E], bSet.OutputOptions[E])
    type Output[E >: PluginError] = (aSet.Output[E], bSet.Output[E])

    given outputOptionsDecoder[E >: PluginError]: PluginSetUtil.PartialOptionDecoder[OutputOptions[E]] =
      PluginSetUtil.PartialOptionDecoderUnion[aSet.OutputOptions[E], bSet.OutputOptions[E]]

    given outputHandler[E >: PluginError]: OutputHandler[E, Output[E]] =
      PluginSetUtil.OutputHandlerUnion[E, aSet.Output[E], bSet.Output[E]]()

    def emitTube
    (context: Ctx)
    (tube: ArTubeC & HasContext[context.type])
    (options: OutputOptions[context.Error])
    : context.Comp[Output[context.Error]] =
      for
        aOut <- aSet.emitTube(context)(tube)(options._1)
        bOut <- bSet.emitTube(context)(tube)(options._2)
      yield (aOut, bOut)

    def extract[Emitter <: TubeEmitter[Ctx]](using TypeTest[TubeEmitter[Ctx], Emitter]): Option[Emitter] =
      aSet.extract[Emitter].orElse(bSet.extract[Emitter])
  }

  object Union {
    def apply[Ctx <: PluginCompatibleContext](a: TubeEmitterSet[Ctx], b: TubeEmitterSet[Ctx]): Union[Ctx, a.type, b.type] =
      new Union[Ctx, a.type, b.type] {
        override val aSet: a.type = a
        override val bSet: b.type = b
      }
  }
}
