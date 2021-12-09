package dev.argon.io

import zio._
import zio.stream._

trait ResourceLoader[+T <: Resource] {
  def load(id: ResourceId): T
}

object BinaryResourceLoader extends ResourceLoader[BinaryResource] {
  override def load(id: ResourceId): BinaryResource = new BinaryResource {
    override def asBytes: UStream[Byte] = id.asStream
  }
}

object TextResourceLoader extends ResourceLoader[TextResource] {
  override def load(id: ResourceId): TextResource = new TextResource {
    override def asText: UStream[String] = ZPipeline.utf8Decode(id.asStream)
    override def asBytes: UStream[Byte] = id.asStream
  }
}
