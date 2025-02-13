package dev.argon.source

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
  given [E >: CharacterCodingException | SyntaxError]: BinaryResourceDecoder[ArgonSourceCodeResource, E] with
    override def decode(resource: BinaryResource[E]): ArgonSourceCodeResource[E] =
      resource match {
        case resource: ArgonSourceCodeResource[E] => resource
        case _ =>
          new ArgonSourceCodeResource[E]:
            override def parsed: ZStream[Any, E, WithSource[Stmt]] =
              resource.decode[TextResource]
                .asText
                .mapChunks { strings => strings.flatMap(_.toCharArray.nn) }
                .via(ArgonSourceParser.parse(fileName))

            override def asText: ZStream[Any, E, String] =
              resource.decode[TextResource]
                .asText

            override def asBytes: Stream[E, Byte] = 
              resource.asBytes

            override def fileName: Option[String] = resource.fileName
          end new
      }
  end given
end ArgonSourceCodeResource

