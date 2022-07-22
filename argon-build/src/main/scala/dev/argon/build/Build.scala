package dev.argon.build

import dev.argon.io.ResourceFactory
import dev.argon.options.OptionDecoder
import dev.argon.util.toml.Toml
import zio.*

object Build {
  def build[R, E >: BuildError, RF <: ResourceFactory[R, E]: Tag](buildConfig: Toml): ZIO[R & RF, E, Unit] =
    summon[OptionDecoder[R, E, BuildConfig[R, E]]].decode(buildConfig)
      .mapError(BuildConfigParseError.apply)
      .unit
}
