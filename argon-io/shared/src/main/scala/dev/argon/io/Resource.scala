package dev.argon.io

import zio.*
import zio.stream.*
import java.io.IOException

trait Resource

trait BinaryResource extends Resource {
  def asBytes: Stream[IOException, Byte]
}

trait TextResource extends BinaryResource {
  def asText: Stream[IOException, String]

  override def asBytes: Stream[IOException, Byte] = ZPipeline.utf8Encode(asText)
}

trait ZipFileResource extends BinaryResource {
  def zipFile: Managed[IOException, ZipFile[Any, IOException]]

  override def asBytes: Stream[IOException, Byte] = ZStream.unwrapManaged(zipFile.map(ZipFilePlatform.serializeZipFile))
}
