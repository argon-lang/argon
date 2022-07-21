package dev.argon.compiler.source

import dev.argon.io.*
import dev.argon.parser.SyntaxError
import dev.argon.parser.tubespec.ModulePatternMapping
import dev.argon.parser.impl.ArgonSourceParser
import zio.stream.*

import java.nio.charset.CharacterCodingException

trait ArgonTubeSpecResource[-R, +E] extends Resource[R, E]:
  def tubeSpec: ZStream[R, E, ModulePatternMapping]
end ArgonTubeSpecResource

object ArgonTubeSpecResource:
  given BinaryResourceDecoder[ArgonTubeSpecResource, Any, CharacterCodingException | SyntaxError] with
    override def decode[R, E >: CharacterCodingException | SyntaxError](resource: BinaryResource[R, E]): ArgonTubeSpecResource[R, E] =
      new ArgonTubeSpecResource[R, E]:
        override def tubeSpec: ZStream[R, E, ModulePatternMapping] =
          (
            summon[BinaryResourceDecoder[TextResource, R, E]]
              .decode(resource)
              .asText
              .mapChunks { strings => strings.flatMap(_.toCharArray) }
              .toChannel
              >>> ArgonSourceParser.parseTubeSpec[E]
            ).toStream

        override def fileName: Option[String] = resource.fileName
      end new
  end given

end ArgonTubeSpecResource


