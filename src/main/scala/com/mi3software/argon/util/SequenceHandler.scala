package com.mi3software.argon.util

import Function.const

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

  final def forCollectedItems[U](f: PartialFunction[U, TItem]): SequenceHandler[U, TTerminator, TResult] =
    new SequenceHandler[U, TTerminator, TResult] {
      override type TState = SequenceHandler.this.TState

      override def initialState: TState = SequenceHandler.this.initialState

      override def next(item: U, state: TState): TState =
        f andThen { mappedItem => SequenceHandler.this.next(mappedItem, state) } applyOrElse (item, const(state))

      override def end(terminator: TTerminator, state: TState): TResult =
        SequenceHandler.this.end(terminator, state)
    }

}
