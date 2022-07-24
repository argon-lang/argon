package dev.argon.build

import dev.argon.io.ResourceFactory
import dev.argon.options.OptionDecoder
import dev.argon.util.toml.{Toml, TomlCodec}
import zio.*

object Build {
  def build[R, E >: BuildError, RF <: ResourceFactory[R, E]: Tag](buildConfig: Toml): ZIO[R & RF, E, Unit] =
    ZIO.fromEither(summon[TomlCodec[BuildConfig]].decode(buildConfig))
      .mapError(BuildConfigParseError.apply)
      .unit
}
