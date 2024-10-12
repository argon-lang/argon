package dev.argon.plugin.adapter

import dev.argon.plugin.*
import dev.argon.options.OptionDecoder
import dev.argon.plugin.scalaApi

class AdaptedPlatformPlugin[Externs, E >: PluginError, PO](
  override val pluginId: String,
  plugin: scalaApi.PlatformPlugin[Externs, E, PO]
) extends PlatformPlugin[E] {
  type PlatformOptions = PO

  override def optionDecoder: OptionDecoder[PO] =
    AdaptedOptionDecoder(plugin.optionDecoder())

  override val externFunction: Extern.Tagged = ???

  override val externRecord: Extern.TaggedRef = ???

  override def emitter[Ctx <: ContextIncluding]: Option[TubeEmitter[Ctx]] = ???

  override def tubeLoaders[Ctx <: ContextOnlyIncluding]: Map[String, TubeLoader[Ctx]] = ???

  
}
