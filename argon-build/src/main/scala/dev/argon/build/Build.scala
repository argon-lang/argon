package dev.argon.build

import dev.argon.io.ResourceFactory
import dev.argon.options.OptionDecoder
import dev.argon.util.toml.Toml
import zio.*

object Build {
  def build[R, E >: BuildError](buildConfig: Toml)(using ResourceFactory[R, E]): ZIO[R, E, Unit] =
    ZIO.fromEither(summon[OptionDecoder[R, E, BuildConfig[R, E]]].decode(buildConfig))
      .mapError(BuildConfigParseError.apply)
      .unit
}
