package dev.argon.plugins.js

import dev.argon.io.{BinaryResource, BinaryResourceDecoder, TextResource}
import dev.argon.plugins.js.estree.*
import zio.*
import zio.stream.*

import java.nio.charset.CharacterCodingException

trait JSProgramResource[-R, +E] extends TextResource[R, E] {
  def asModule: ZIO[R, E, Program]
}

object JSProgramResource:
  trait Impl[-R, +E >: JSGenerateException | CharacterCodingException] extends JSProgramResource[R, E] with TextResource.Impl[R, E]:
    override def asText: ZStream[R, E, String] =
      ZStream.fromZIO(asModule.flatMap { module =>
        ZIO.scoped(JSContext.make.flatMap(_.generate(module)))
      })
  end Impl


  given BinaryResourceDecoder[JSProgramResource, Any, JSPluginError] with
    def decode[R <: Any, E >: JSPluginError](resource: BinaryResource[R, E]): JSProgramResource[R, E] =
      summon[BinaryResourceDecoder[TextResource, R, E]].decode(resource) match {
        case resource: JSProgramResource[R, E] => resource
        case resource =>
          new JSProgramResource.Impl[R, E] {
            override def asModule: ZIO[R, E, Program] =
              resource
                .asText
                .runCollect
                .flatMap { text =>
                  ZIO.scoped(
                    JSContext.make.flatMap { ctx =>
                      ctx.parse(resource.fileName.getOrElse("file.js"), text.mkString)
                    }
                  )
                }

            override def fileName: Option[String] = resource.fileName
          }

      }
  end given
end JSProgramResource

