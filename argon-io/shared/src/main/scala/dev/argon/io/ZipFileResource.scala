package dev.argon.io

import zio.*
import zio.stream.*

import java.io.IOException


trait ZipFileResource[-R, +E] extends BinaryResource[R, E] {
  def asZip: ZIO[R & Scope, E, ZipFileResource.Zip[R, E]]
}

object ZipFileResource:
  trait Impl[-R, +E >: IOException] extends ZipFileResource[R, E] with ZipFileResourceImplPlatformSpecific[R, E]

  trait Zip[-R, +E]:
    def getEntry(path: String): ZIO[R & Scope, E, Option[Entry[R, E]]]
    def entries: ZStream[R, E, Entry[R, E]]
  end Zip

  trait Entry[-R, +E]:
    val path: String
    def value: BinaryResource[R, E]
  end Entry

  given BinaryResourceDecoder[ZipFileResource, Any, IOException] =
    ZipFileDecoderPlatformSpecific()

end ZipFileResource
