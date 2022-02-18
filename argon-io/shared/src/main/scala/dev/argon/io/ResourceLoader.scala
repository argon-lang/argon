package dev.argon.io

import zio.*
import zio.stream.*
import java.io.IOException

trait ResourceLoader[+T] {
  def loadResourceId: PartialFunction[ResourceId, T]
  def loadStream(stream: Stream[IOException, Byte]): T
}

final class BinaryResourceLoader extends ResourceLoader[BinaryResource] {

  override def loadResourceId: PartialFunction[ResourceId, BinaryResource] = PartialFunction.empty

  override def loadStream(stream: Stream[IOException, Byte]): BinaryResource =
    new BinaryResource {
      override def asBytes: Stream[IOException, Byte] = stream
    }

}

object TextResourceLoader extends ResourceLoader[TextResource] {

  override def loadResourceId: PartialFunction[ResourceId, TextResource] = PartialFunction.empty

  override def loadStream(stream: Stream[IOException, Byte]): TextResource =
    new TextResource {
      override def asText: Stream[IOException, String] = ZPipeline.utf8Decode.apply(stream)
      override def asBytes: Stream[IOException, Byte] = stream
    }

}
