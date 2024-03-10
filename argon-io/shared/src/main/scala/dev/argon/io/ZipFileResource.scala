package dev.argon.io

import zio.*
import zio.stream.*

import java.io.IOException


trait ZipFileResource[+E] extends ZipStreamResource[E] {
  override def asZip: ZIO[Scope, E, ZipFileResource.Zip[E]]
}

object ZipFileResource:
  trait Impl[+E >: IOException] extends ZipFileResource[E]

  trait Zip[+E] extends ZipStreamResource.Zip[E]:
    def getEntry(path: String): ZIO[Scope, E, Option[ZipStreamResource.Entry[E]]]
  end Zip

  given BinaryResourceDecoder[ZipFileResource, IOException] =
    ZipFileDecoderPlatformSpecific()

end ZipFileResource
