package dev.argon.io

import zio.*
import zio.stream.*
import java.io.IOException


object BinaryResourceLoader extends BinaryResourceLoaderPlatformSpecific {
  def loadStream[R, E](stream: ZStream[R, E, Byte]): BinaryResource[R, E] =
    new BinaryResource[R, E] with Resource.WithoutFileName {
      override def asBytes: ZStream[R, E, Byte] = stream
    }

}
