package dev.argon.plugin.tube

import dev.argon.options.*
import dev.argon.plugin.*
import dev.argon.util.*
import dev.argon.io.*
import zio.*

object TubeBackend extends Backend[Nothing] {
  override type OutputOptionID = TubeOutputOptionID
  override val outputOptions: OptionsHandler[OutputOptionID, [_] =>> SingleFile] = new OptionsHandlerImpl[OutputOptionID, [_] =>> SingleFile]
  override val outputResourceTags: Options[ResourceTag, OutputOptionID] = Options.fromFunction([E] => (id: OutputOptionID with TypedOptionID[E]) =>
    id match {
      case TubeOutputOptionID.ImplementationTube => TubeImplementationResourceTag
    } : ResourceTag[E]
  )

  override def emitModule(options: Options[Id, Nothing])(tube: SerializedTube): Options[Id, OutputOptionID] = ???
}

sealed trait TubeOutputOptionID extends OptionID derives CanEqual {
  override type ElementType <: Resource
  override type Decoded[_] = SingleFile
}

object TubeOutputOptionID {
  case object ImplementationTube extends OptionIDBase[[_] =>> SingleFile, TubeImplementationResource] with TubeOutputOptionID:
    override val info: OptionInfo[TubeImplementationResource] =
      OptionInfo(
        name = "tube.implementation",
        description = "Tube contaning implementation of all methods",
      )
}

