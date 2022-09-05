package dev.argon.plugins.js

import dev.argon.compiler.module.ModulePath
import dev.argon.compiler.tube.TubeName
import dev.argon.util.{*, given}
import dev.argon.io.*
import dev.argon.options.*
import dev.argon.util.toml.{Toml, TomlCodec}
import zio.ZIO

import java.io.IOException
import java.nio.charset.CharacterCodingException

final case class JSOptions[-R, +E]
(
  extern: Option[Seq[JSProgramResource[R, E]]],
  modules: JSModuleOptionsMap[R, E],
  tubes: JSTubeOptionsMap,
)

object JSOptions:

  given optionDecoder[R, E >: JSPluginError]: OptionCodec[R, E, JSOptions[R, E]] =
    OptionCodec.derive

end JSOptions

final case class JSModuleOptions[-R, +E]
(
  inject_before: Option[JSProgramResource[R, E]],
  inject_after: Option[JSProgramResource[R, E]],
)

object JSModuleOptions:

  given optionDecoder[R, E >: JSPluginError]: OptionCodec[R, E, JSModuleOptions[R, E]] =
    OptionCodec.derive

end JSModuleOptions

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
            (tubeName.urlEncode, summon[TomlCodec[JSTubeOptions]].encode(options))
        }
      )

    def decode(toml: Toml): Either[String, JSTubeOptionsMap] =
      toml match {
        case Toml.Table(map) =>
          map
            .toSeq
            .traverse { case (key, value) =>
              for
                name <- TubeName.urlDecode(key).toRight { s"Key was empty: $key" }
                options <- summon[TomlCodec[JSTubeOptions]].decode(value)
              yield (name, options)
            }
            .map { optMap =>
              JSTubeOptionsMap(optMap.toMap)
            }

        case _ =>
          Left("Expected table")
      }

    override def defaultValue: Option[JSTubeOptionsMap] = Some(JSTubeOptionsMap(Map.empty))
  end given
end JSTubeOptionsMap


final case class JSModuleOptionsMap[-R, +E]
(
  map: Map[ModulePath, JSModuleOptions[R, E]],
)

object JSModuleOptionsMap:
  given [R, E >: JSPluginError]: OptionCodec[R, E, JSModuleOptionsMap[R, E]] with
    override def decode(value: Toml): ZIO[ResourceFactory, String, JSModuleOptionsMap[R, E]] =
      value match {
        case Toml.Table(map) =>
          map
            .toSeq
            .traverse { case (key, value) =>
              for
                options <- summon[OptionCodec[R, E, JSModuleOptions[R, E]]].decode(value)
              yield (ModulePath.urlDecode(key), options)
            }
            .map { optMap =>
              JSModuleOptionsMap(optMap.toMap)
            }

        case _ =>
          ZIO.fail("Expected table")
      }

    override def encode(recorder: ResourceRecorder[R, E])(value: JSModuleOptionsMap[R, E]): ZIO[R, E, Toml] =
      ZIO.foreach(value.map.toSeq) { (path, options) =>
        summon[OptionCodec[R, E, JSModuleOptions[R, E]]].encode(recorder)(options)
          .map { convOpts =>
            path.urlEncode -> convOpts
          }
      }.map { conv => Toml.Table(conv.toMap) }

    override def defaultValue: Option[JSModuleOptionsMap[Any, E]] = Some(JSModuleOptionsMap(Map.empty))
  end given
end JSModuleOptionsMap

