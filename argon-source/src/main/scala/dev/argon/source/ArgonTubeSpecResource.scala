package dev.argon.source

import dev.argon.io.*
import dev.argon.parser.{ArgonSourceParser, SyntaxError}
import dev.argon.ast.ModulePatternMapping
import zio.stream.*

import java.nio.charset.CharacterCodingException

abstract class ArgonTubeSpecResource[+E] extends TextResource[E]:
  def tubeSpec: ZStream[Any, E, ModulePatternMapping]
end ArgonTubeSpecResource

object ArgonTubeSpecResource:
  given [E >: CharacterCodingException | SyntaxError]: BinaryResourceDecoder[ArgonTubeSpecResource, E] with
    override def decode(resource: BinaryResource[E]): ArgonTubeSpecResource[E] =
      new ArgonTubeSpecResource[E]:
        override def tubeSpec: ZStream[Any, E, ModulePatternMapping] =
          resource.decode[TextResource]
            .asText
            .mapChunks { strings => strings.flatMap(_.toCharArray.nn) }
            .via(ArgonSourceParser.parseTubeSpec(fileName))

        override def asText: ZStream[Any, E, String] =
          resource.decode[TextResource]
            .asText

        override def asBytes: Stream[E, Byte] =
          resource.asBytes

        override def fileName: Option[String] = resource.fileName
      end new
  end given

end ArgonTubeSpecResource


