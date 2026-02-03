package dev.argon.source

import dev.argon.ast.ModuleDeclaration
import dev.argon.io.*
import dev.argon.parser.{ArgonSourceParser, SyntaxError}
import zio.ZIO
import zio.stream.*

import java.nio.charset.CharacterCodingException

abstract class ArgonSourceCodeResource[+E] extends TextResource[E]:
  def parsed: ZIO[Any, E, ModuleDeclaration]
end ArgonSourceCodeResource

object ArgonSourceCodeResource:
  given [E >: CharacterCodingException | SyntaxError] => BinaryResourceDecoder[ArgonSourceCodeResource, E]:
    override def decode(resource: BinaryResource[E]): ArgonSourceCodeResource[E] =
      resource match {
        case resource: ArgonSourceCodeResource[E] => resource
        case _ =>
          new ArgonSourceCodeResource[E]:
            override def parsed: ZIO[Any, E, ModuleDeclaration] =
              ArgonSourceParser.parse[Any, E](fileName, resource.decode[TextResource].asText)

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

