package com.mi3software.argon.util

import scala.io.Source
import scalaz.StreamT
import scalaz.effect.IO

object FileStreamer {

  private def createStream(source: Source): StreamT[IO, Char] =
    StreamT[IO, Char](IO {
      if(source.hasNext)
        StreamT.Yield(source.next(), createStream(source))
      else
        StreamT.Done
    })

  def streamFile[T](source: => Source)(f: StreamT[IO, Char] => IO[T]): IO[T] =
    IO { source }
      .flatMap(createStream _ andThen f)
      .ensuring(IO {
        source.close()
      })

}
