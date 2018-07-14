package com.mi3software.argon.util

import scala.io.Source
import scalaz.effect.IO

object FileSequenceHandlerProvider {

  def provideFile[TResult](source: IO[Source])(sequenceHandler: SequenceHandler[Char, Any, TResult]): IO[TResult] =
    source.flatMap { s =>
      IO {
        val endState = s.toStream.foldLeft(sequenceHandler.initialState) { (state, ch) => sequenceHandler.next(ch, state) }
        sequenceHandler.end((), endState)
      }
        .ensuring(IO { s.close() })
    }



}
