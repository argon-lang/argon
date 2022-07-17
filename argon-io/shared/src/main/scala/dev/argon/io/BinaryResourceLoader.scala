package dev.argon.io

import zio.*
import zio.stream.*
import java.io.IOException


object BinaryResourceLoader extends BinaryResourceLoaderPlatformSpecific {
  def loadStream[E](stream: Stream[E, Byte]): BinaryResource[E] =
    new BinaryResource[E] {
      override def asBytes: Stream[E, Byte] = stream
      override def fileName: Option[String] = None
    }

}
