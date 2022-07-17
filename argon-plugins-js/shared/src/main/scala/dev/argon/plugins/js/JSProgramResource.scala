package dev.argon.plugins.js

import dev.argon.io.{BinaryResourceDecoder, BinaryResource, TextResource}
import dev.argon.plugins.js.estree.*
import zio.*
import zio.stream.*

trait JSProgramResource[+E] extends TextResource[E] {
  def asModule: IO[E, Program]

  override def asText: Stream[E, String] =
    ZStream.fromZIO(asModule.flatMap { module =>
      ZIO.scoped(JSContext.make.flatMap(_.generate(module)))
    })
}

object JSProgramResource:
  given BinaryResourceDecoder[JSProgramResource] with
    def decode[E](resource: BinaryResource[E]): JSProgramResource[E] =
      resource match {
        case resource: JSProgramResource[E] => resource
        case _ => new JSProgramResource[E]:
          override def asModule: IO[E, Program] = ???
      }
  end given
end JSProgramResource

