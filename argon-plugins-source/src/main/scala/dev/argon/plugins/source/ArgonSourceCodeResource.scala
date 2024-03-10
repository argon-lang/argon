package dev.argon.plugins.source

import dev.argon.ast.Stmt
import dev.argon.compiler.*
import dev.argon.io.*
import dev.argon.parser.{ArgonSourceParser, SyntaxError}
import dev.argon.util.WithSource
import zio.stream.*

import java.io.IOException
import java.nio.charset.CharacterCodingException

abstract class ArgonSourceCodeResource[+E] extends TextResource[E]:
  def parsed: ZStream[Any, E, WithSource[Stmt]]
end ArgonSourceCodeResource

object ArgonSourceCodeResource:
  given BinaryResourceDecoder[ArgonSourceCodeResource, CharacterCodingException | ArgonSyntaxError] with
    override def decode[E >: CharacterCodingException | ArgonSyntaxError](resource: BinaryResource[E]): ArgonSourceCodeResource[E] =
      new ArgonSourceCodeResource[E] with TextResource.Impl[E]:
        override def parsed: ZStream[Any, E, WithSource[Stmt]] =
          summon[BinaryResourceDecoder[TextResource, E]]
            .decode(resource)
            .asText
            .mapChunks { strings => strings.flatMap(_.toCharArray.nn) }
            .via(ArgonSourceParser.parse(fileName).mapError(ArgonSyntaxError(_)))

        override def asText: ZStream[Any, E, String] =
          summon[BinaryResourceDecoder[TextResource, E]]
            .decode(resource)
            .asText

        override def fileName: Option[String] = resource.fileName
      end new
  end given
end ArgonSourceCodeResource

