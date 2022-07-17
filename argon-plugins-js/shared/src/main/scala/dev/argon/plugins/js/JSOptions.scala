package dev.argon.plugins.js

import dev.argon.util.*
import dev.argon.io.*
import dev.argon.options.*
import dev.argon.options.OptionDecoder.ResourceFactory
import dev.argon.util.toml.Toml

import java.io.IOException

final case class JSOptions[Res[_[_]]]
(
  header: Option[Res[JSProgramResource]],
  footer: Option[Res[JSProgramResource]],
  extern: Seq[Res[JSProgramResource]],
)

object JSOptions:

  object Handler extends OptionHandler[JSOptions]:
    override def optionDecoder[E >: ResourceDecodeException](using ResourceFactory[E]): OptionDecoder[OptionHandler.WithRes[JSOptions, E], E] =
      OptionDecoder.derive[JSOptions[[Res[_]] =>> Res[E]] , E]
  end Handler


end JSOptions

