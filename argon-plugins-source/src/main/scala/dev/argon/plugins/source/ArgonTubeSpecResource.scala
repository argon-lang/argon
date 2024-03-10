package dev.argon.plugins.source

import dev.argon.io.*
import dev.argon.parser.{ArgonSourceParser, SyntaxError}
import dev.argon.ast.ModulePatternMapping
import zio.stream.*

import java.nio.charset.CharacterCodingException

abstract class ArgonTubeSpecResource[+E] extends TextResource[E]:
  def tubeSpec: ZStream[Any, E, ModulePatternMapping]
end ArgonTubeSpecResource

object ArgonTubeSpecResource:
  given BinaryResourceDecoder[ArgonTubeSpecResource, CharacterCodingException | SyntaxError] with
    override def decode[E >: CharacterCodingException | SyntaxError](resource: BinaryResource[E]): ArgonTubeSpecResource[E] =
      new ArgonTubeSpecResource[E] with TextResource.Impl[E]:
        override def tubeSpec: ZStream[Any, E, ModulePatternMapping] =
          summon[BinaryResourceDecoder[TextResource, E]]
            .decode(resource)
            .asText
            .mapChunks { strings => strings.flatMap(_.toCharArray.nn) }
            .via(ArgonSourceParser.parseTubeSpec(fileName))

        override def asText: ZStream[Any, E, String] =
          summon[BinaryResourceDecoder[TextResource, E]]
            .decode(resource)
            .asText

        override def fileName: Option[String] = resource.fileName
      end new
  end given

end ArgonTubeSpecResource


