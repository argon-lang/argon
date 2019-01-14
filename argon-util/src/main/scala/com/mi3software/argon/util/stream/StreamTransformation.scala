package com.mi3software.argon.util.stream

import scalaz.Monad

trait StreamTransformation[A, R, B, R2] {

  protected type S
  protected val initialState: S

  protected def processItem(state: S, item: A): (S, Vector[B])
  protected def processResult(state: S, result: R): (R2, Vector[B])

  final def transformStream[F[_]: Monad](stream: ArStream[F, A, R]): ArStream[F, B, R2] =
    stream.transform(initialState)(processResult)(processItem)

}
