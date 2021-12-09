package dev.argon.io

import zio._
import zio.stream._

trait Resource

trait BinaryResource extends Resource {
  def asBytes: UStream[Byte]
}

trait TextResource extends BinaryResource {
  def asText: UStream[String]
  
  override def asBytes: UStream[Byte] =
    ZPipeline.utf8Encode(asText)
}

trait ZipFileResource extends BinaryResource {
  def zipFile: UManaged[ZipFile]

  override def asBytes: UStream[Byte] =
    ZStream.unwrapManaged(zipFile.map(ZipFilePlatform.serializeZipFile))
}

