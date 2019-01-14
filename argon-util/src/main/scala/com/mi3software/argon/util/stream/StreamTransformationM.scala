package com.mi3software.argon.util.stream

trait StreamTransformationM[F[_], A, R, B, R2] {

  protected type S
  protected val initialState: S

  protected def processItem(state: S, item: A): F[(S, Vector[B])]
  protected def processResult(state: S, result: R): F[(R2, Vector[B])]

  final def transformStream(stream: ArStream[F, A, R]): ArStream[F, B, R2] =
    stream.transformM(initialState)(processResult)(processItem)

}
