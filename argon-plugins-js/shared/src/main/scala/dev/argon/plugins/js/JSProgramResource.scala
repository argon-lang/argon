package dev.argon.plugins.js

import dev.argon.io.{BinaryResource, BinaryResourceDecoder, ResourceDecodeException, TextResource}
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
    def decode[E >: ResourceDecodeException](resource: BinaryResource[E]): JSProgramResource[E] =
      summon[BinaryResourceDecoder[TextResource]].decode(resource) match {
        case resource: JSProgramResource[E] => resource
        case resource =>
          new JSProgramResource[E] {
            override def asModule: IO[E, Program] =
              resource
                .asText
                .runCollect
                .flatMap { text =>
                  ZIO.scoped(
                    JSContext.make.flatMap { ctx =>
                      ctx.parse(resource.fileName.getOrElse("file.js"), text.mkString)
                        .mapError { err => ResourceDecodeException("JS Parse Error: " + err) }
                    }
                  )
                }

            override def fileName: Option[String] = resource.fileName
          }

      }
  end given
end JSProgramResource

