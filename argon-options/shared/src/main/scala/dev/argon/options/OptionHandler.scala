package dev.argon.options

import dev.argon.io.ResourceDecodeException
import dev.argon.options.OptionDecoder.ResourceFactory
import dev.argon.util.*
import dev.argon.util.toml.TomlCodec

trait OptionHandler[Options[_[_[_]]]] {
  def optionDecoder[E >: ResourceDecodeException](using ResourceFactory[E]): OptionDecoder[OptionHandler.WithRes[Options, E], E]
}

object OptionHandler {
  type WithRes[Options[_[_[_]]], E] = Options[[Res[_]] =>> Res[E]]
}



