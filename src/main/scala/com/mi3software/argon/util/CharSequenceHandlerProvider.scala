package com.mi3software.argon.util

object CharSequenceHandlerProvider {

  def provideString[TResult](str: String)(sequenceHandler: SequenceHandler[Char, Any, TResult]): TResult =
    sequenceHandler.end((), str.foldLeft(sequenceHandler.initialState) { (state, ch) => sequenceHandler.next(ch, state) })



}
