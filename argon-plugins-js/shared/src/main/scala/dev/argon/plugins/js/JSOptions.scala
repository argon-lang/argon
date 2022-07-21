package dev.argon.plugins.js

import dev.argon.compiler.tube.TubeName
import dev.argon.util.{*, given}
import dev.argon.io.*
import dev.argon.options.*
import dev.argon.util.toml.{Toml, TomlCodec}

import java.io.IOException
import java.nio.charset.CharacterCodingException

final case class JSOptions[R, E]
(
  header: Option[JSProgramResource[R, E]],
  footer: Option[JSProgramResource[R, E]],
  extern: Seq[JSProgramResource[R, E]],

  tubes: JSTubeOptionsMap,
)

object JSOptions:

  given optionDecoder[R, E >: JSPluginError](using ResourceFactory[R, E]): OptionDecoder[R, E, JSOptions[R, E]] =
    OptionDecoder.derive

end JSOptions

final case class JSTubeOptions
(
  import_path: String,
) derives TomlCodec

final case class JSTubeOptionsMap
(
  map: Map[TubeName, JSTubeOptions],
)

object JSTubeOptionsMap:
  given TomlCodec[JSTubeOptionsMap] with
    def encode(a: JSTubeOptionsMap): Toml =
      Toml.Table(
        a.map.map {
          case (tubeName, options) =>
            (tubeName.name.toList.mkString("."), summon[TomlCodec[JSTubeOptions]].encode(options))
        }
      )

    def decode(toml: Toml): Either[String, JSTubeOptionsMap] =
      toml match {
        case Toml.Table(map) =>
          map
            .toSeq
            .traverse { case (key, value) =>
              for
                name <- NonEmptyList.fromList(key.split(".").toList).toRight { "Key was empty" }
                options <- summon[TomlCodec[JSTubeOptions]].decode(value)
              yield (TubeName(name), options)
            }
            .map { optMap =>
              JSTubeOptionsMap(optMap.toMap)
            }

        case _ =>
          Left("Expected table")
      }

  end given
end JSTubeOptionsMap

