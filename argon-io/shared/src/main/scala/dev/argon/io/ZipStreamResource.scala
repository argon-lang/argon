package dev.argon.io

import zio.*
import zio.stream.ZStream

import java.io.IOException

trait ZipStreamResource[+E] extends BinaryResource[E] {
  def asZip: ZIO[Scope, E, ZipStreamResource.Zip[E]]
}

object ZipStreamResource:
  trait Impl[+E >: IOException] extends ZipStreamResource[E] with ZipStreamResourceImplPlatformSpecific[E]

  trait Zip[+E]:
    def entries: ZStream[Any, E, Entry[E]]
  end Zip

  trait Entry[+E]:
    val path: String
    def value: BinaryResource[E]
  end Entry

  given BinaryResourceDecoder[ZipFileResource, IOException] =
    ZipFileDecoderPlatformSpecific()

end ZipStreamResource
