package com.mi3software.argon.util.stream

import scalaz._
import Scalaz._
import com.mi3software.argon.util.NonEmptyVector

trait StreamTransformationM[F[_], A, R, B, R2] {

  protected type S
  protected val initialState: S

  protected def processItems(state: S, items: NonEmptyVector[A])(implicit monadInstance: Monad[F]): F[(S, Vector[B])]
  protected def processResult(state: S, result: R)(implicit monadInstance: Monad[F]): F[(R2, Vector[B])]

  final def transformStream(stream: ArStream[F, A, R])(implicit monadInstance: Monad[F]): ArStream[F, B, R2] =
    stream.transformM(initialState)(processResult)(processItems)

  final def mapItems[B2](f: B => B2): StreamTransformationM[F, A, R, B2, R2] = new StreamTransformationM[F, A, R, B2, R2] {
    override protected type S = StreamTransformationM.this.S

    override protected val initialState: S = StreamTransformationM.this.initialState

    override protected def processItems(state: S, items: NonEmptyVector[A])(implicit monadInstance: Monad[F]): F[(S, Vector[B2])] =
      StreamTransformationM.this.processItems(state, items).map { case (state2, outItems) => (state2, outItems.map(f)) }

    override protected def processResult(state: S, result: R)(implicit monadInstance: Monad[F]): F[(R2, Vector[B2])] =
      StreamTransformationM.this.processResult(state, result).map { case (state2, outItems) => (state2, outItems.map(f)) }
  }


  final def flatMapItems[B2](f: B => Vector[B2]): StreamTransformationM[F, A, R, B2, R2] = new StreamTransformationM[F, A, R, B2, R2] {
    override protected type S = StreamTransformationM.this.S

    override protected val initialState: S = StreamTransformationM.this.initialState

    override protected def processItems(state: S, items: NonEmptyVector[A])(implicit monadInstance: Monad[F]): F[(S, Vector[B2])] =
      StreamTransformationM.this.processItems(state, items).map { case (state2, outItems) => (state2, outItems.flatMap(f)) }

    override protected def processResult(state: S, result: R)(implicit monadInstance: Monad[F]): F[(R2, Vector[B2])] =
      StreamTransformationM.this.processResult(state, result).map { case (state2, outItems) => (state2, outItems.flatMap(f)) }
  }


  final def mapResult[R3](f: R2 => R3): StreamTransformationM[F, A, R, B, R3] = new StreamTransformationM[F, A, R, B, R3] {
    override protected type S = StreamTransformationM.this.S

    override protected val initialState: S = StreamTransformationM.this.initialState

    override protected def processItems(state: S, items: NonEmptyVector[A])(implicit monadInstance: Monad[F]): F[(S, Vector[B])] =
      StreamTransformationM.this.processItems(state, items)

    override protected def processResult(state: S, result: R)(implicit monadInstance: Monad[F]): F[(R3, Vector[B])] =
      StreamTransformationM.this.processResult(state, result).map { case (state2, outItems) => (f(state2), outItems) }
  }

}

object StreamTransformationM {
  trait Single[F[_], A, R, B, R2] extends StreamTransformationM[F, A, R, B, R2] {
    protected def processItem(state: S, item: A): F[(S, Vector[B])]

    override protected def processItems(state: S, items: NonEmptyVector[A])(implicit monadInstance: Monad[F]): F[(S, Vector[B])] =
      items.foldLeftM((state, Vector.empty[B])) {
        case ((state, acc), item) =>
          processItem(state, item).map {
            case (state2, newItems) => (state2, acc ++ newItems)
          }
      }
  }
}
