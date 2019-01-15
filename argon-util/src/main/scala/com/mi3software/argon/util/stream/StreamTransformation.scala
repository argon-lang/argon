package com.mi3software.argon.util.stream

import com.mi3software.argon.util.NonEmptyVector
import scalaz._
import Scalaz._

trait StreamTransformation[A, R, B, R2] {

  protected type S
  protected val initialState: S

  protected def processItems(state: S, items: NonEmptyVector[A]): (S, Vector[B])
  protected def processResult(state: S, result: R): (R2, Vector[B])

  final def transformStream[F[_]: Monad](stream: ArStream[F, A, R]): ArStream[F, B, R2] =
    stream.transform(initialState)(processResult)(processItems)

}

object StreamTransformation {

  trait Single[A, R, B, R2] extends StreamTransformation[A, R, B, R2] {
    protected def processItem(state: S, item: A): (S, Vector[B])

    final override protected def processItems(state: S, items: NonEmptyVector[A]): (S, Vector[B]) =
      items.foldLeft((state, Vector.empty[B])) {
        case ((state, acc), item) =>
          val (state2, newItems) = processItem(state, item)
          (state2, acc ++ newItems)
      }
  }

}
