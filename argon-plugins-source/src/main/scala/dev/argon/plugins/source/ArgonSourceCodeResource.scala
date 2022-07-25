package dev.argon.plugins.source

import dev.argon.compiler.*
import dev.argon.io.*
import dev.argon.parser.{Stmt, SyntaxError}
import dev.argon.parser.impl.ArgonSourceParser
import zio.stream.*

import java.io.IOException
import java.nio.charset.CharacterCodingException

abstract class ArgonSourceCodeResource[-R, +E] extends Resource[R, E]:
  def parsed: ZStream[R, E, Stmt]
end ArgonSourceCodeResource

object ArgonSourceCodeResource:
  given BinaryResourceDecoder[ArgonSourceCodeResource, Any, CharacterCodingException | SyntaxError] with
    override def decode[R, E >: CharacterCodingException | SyntaxError](resource: BinaryResource[R, E]): ArgonSourceCodeResource[R, E] =
      new ArgonSourceCodeResource[R, E]:
        override def parsed: ZStream[R, E, Stmt] =
          (
            summon[BinaryResourceDecoder[TextResource, R, E]]
              .decode(resource)
              .asText
              .mapChunks { strings => strings.flatMap(_.toCharArray) }
              .toChannel
              >>> ArgonSourceParser.parse[E](fileName)
            ).toStream

        override def fileName: Option[String] = resource.fileName
      end new
  end given
end ArgonSourceCodeResource

