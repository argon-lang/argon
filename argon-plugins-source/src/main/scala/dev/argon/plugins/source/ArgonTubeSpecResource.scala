package dev.argon.plugins.source

import dev.argon.io.*
import dev.argon.parser.SyntaxError
import dev.argon.parser.tubespec.ModulePatternMapping
import dev.argon.parser.impl.ArgonSourceParser
import zio.stream.*

import java.nio.charset.CharacterCodingException

abstract class ArgonTubeSpecResource[-R, +E] extends TextResource[R, E]:
  def tubeSpec: ZStream[R, E, ModulePatternMapping]
end ArgonTubeSpecResource

object ArgonTubeSpecResource:
  given BinaryResourceDecoder[ArgonTubeSpecResource, Any, CharacterCodingException | SyntaxError] with
    override def decode[R, E >: CharacterCodingException | SyntaxError](resource: BinaryResource[R, E]): ArgonTubeSpecResource[R, E] =
      new ArgonTubeSpecResource[R, E] with TextResource.Impl[R, E]:
        override def tubeSpec: ZStream[R, E, ModulePatternMapping] =
          (
            summon[BinaryResourceDecoder[TextResource, R, E]]
              .decode(resource)
              .asText
              .mapChunks { strings => strings.flatMap(_.toCharArray.nn) }
              .toChannel
              >>> ArgonSourceParser.parseTubeSpec[E](fileName)
            ).toStream

        override def asText: ZStream[R, E, String] =
          summon[BinaryResourceDecoder[TextResource, R, E]]
            .decode(resource)
            .asText

        override def fileName: Option[String] = resource.fileName
      end new
  end given

end ArgonTubeSpecResource


