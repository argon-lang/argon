package dev.argon.io

import zio.*
import zio.stream.*

import java.io.IOException


trait ZipFileResource[-R, +E] extends ZipStreamResource[R, E] {
  override def asZip: ZIO[R & Scope, E, ZipFileResource.Zip[R, E]]
}

object ZipFileResource:
  trait Impl[-R, +E >: IOException] extends ZipFileResource[R, E]

  trait Zip[-R, +E] extends ZipStreamResource.Zip[R, E]:
    def getEntry(path: String): ZIO[R & Scope, E, Option[ZipStreamResource.Entry[R, E]]]
  end Zip

  given BinaryResourceDecoder[ZipFileResource, Any, IOException] =
    ZipFileDecoderPlatformSpecific()

end ZipFileResource
