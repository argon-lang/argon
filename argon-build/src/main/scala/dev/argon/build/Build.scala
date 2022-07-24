package dev.argon.build

import dev.argon.io.ResourceFactory
import dev.argon.options.OptionDecoder
import dev.argon.util.toml.{Toml, TomlCodec}
import zio.*

object Build {
  def build[E >: BuildError, RF <: ResourceFactory[E]: Tag, R <: RF](buildConfig: Toml): ZIO[R, E, Unit] =
    ZIO.fromEither(summon[TomlCodec[BuildConfig]].decode(buildConfig))
      .mapError(BuildConfigParseError.apply)
      .unit
}
