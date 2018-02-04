package com.mi3software.argon.util

final class VectorBuilderSequenceHandler[TItem] extends SequenceHandler[TItem, Any, Vector[TItem]] {

  override type TState = Vector[TItem]

  override def initialState: Vector[TItem] = Vector.empty

  override def next(item: TItem, state: Vector[TItem]): Vector[TItem] =
    state :+ item

  override def end(terminator: Any, state: Vector[TItem]): Vector[TItem] =
    state

}
