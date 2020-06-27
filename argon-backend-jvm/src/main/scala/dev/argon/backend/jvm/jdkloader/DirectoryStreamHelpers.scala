package dev.argon.backend.jvm.jdkloader

import java.nio.file.DirectoryStream

import zio.stream._
import zio.{IO, ZManaged}

@SuppressWarnings(Array("dev.argon.warts.ZioEffect"))
private[jvm] object DirectoryStreamHelpers {

  def toZStream[T](stream: => DirectoryStream[T]): Stream[Throwable, T] =
    ZStream.unwrapManaged(
      ZManaged.fromAutoCloseable(IO.effect { stream }).map { dirStream =>
        ZStream.fromJavaIterator(dirStream.iterator())
      }
    )

}
