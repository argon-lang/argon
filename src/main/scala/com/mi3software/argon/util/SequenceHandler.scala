package com.mi3software.argon.util

import scalaz._
import Scalaz._

trait SequenceHandler[@specialized(Int, Char) -TItem, -TTerminator, @specialized(Int, Char) +TResult] {
  type TState

  def initialState: TState
  def next(item: TItem, state: TState): TState
  def end(terminator: TTerminator, state: TState): TResult

  final def map[U](f: TResult => U): SequenceHandler[TItem, TTerminator, U] =
    new SequenceHandler[TItem, TTerminator, U] {
      override type TState = SequenceHandler.this.TState

      override def initialState: TState = SequenceHandler.this.initialState

      override def next(item: TItem, state: TState): TState =
        SequenceHandler.this.next(item, state)

      override def end(terminator: TTerminator, state: TState): U =
        f(SequenceHandler.this.end(terminator, state))
    }

}
