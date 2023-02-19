package dev.argon.io

import zio.*
import zio.stream.ZStream

import java.io.IOException

trait ZipStreamResource[-R, +E] extends BinaryResource[R, E] {
  def asZip: ZIO[R & Scope, E, ZipStreamResource.Zip[R, E]]
}

object ZipStreamResource:
  trait Impl[-R, +E >: IOException] extends ZipStreamResource[R, E] with ZipStreamResourceImplPlatformSpecific[R, E]

  trait Zip[-R, +E]:
    def entries: ZStream[R, E, Entry[R, E]]
  end Zip

  trait Entry[-R, +E]:
    val path: String
    def value: BinaryResource[R, E]
  end Entry

  given BinaryResourceDecoder[ZipFileResource, Any, IOException] =
    ZipFileDecoderPlatformSpecific()

end ZipStreamResource
