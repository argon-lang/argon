package dev.argon.util.stream

import scalaz._
import Scalaz._
import dev.argon.util.NonEmptyVector

trait StreamTransformation[F[_], A, R, B, R2] {

  type S
  val initialState: S

  def processItems[S2](state: S, state2: S2, items: NonEmptyVector[A])(f: (S2, NonEmptyVector[B]) => F[S2])(implicit monadInstance: Monad[F]): F[(S, S2)]
  def processResult[S2](state: S, state2: S2, result: R)(f: (S2, NonEmptyVector[B]) => F[S2])(implicit monadInstance: Monad[F]): F[(R2, S2)]


  final def mapItems[B2](f: B => B2): StreamTransformation[F, A, R, B2, R2] = new StreamTransformation[F, A, R, B2, R2] {
    override type S = StreamTransformation.this.S

    override val initialState: S = StreamTransformation.this.initialState


    override def processItems[S2](state: S, state2: S2, items: NonEmptyVector[A])(f2: (S2, NonEmptyVector[B2]) => F[S2])(implicit monadInstance: Monad[F]): F[(S, S2)] =
      StreamTransformation.this.processItems(state, state2, items) { (s2, bItems) => f2(s2, bItems.map(f)) }

    override def processResult[S2](state: S, state2: S2, result: R)(f2: (S2, NonEmptyVector[B2]) => F[S2])(implicit monadInstance: Monad[F]): F[(R2, S2)] =
      StreamTransformation.this.processResult(state, state2, result) { (s2, bItems) => f2(s2, bItems.map(f)) }

  }


  final def flatMapItems[B2](f: B => Vector[B2]): StreamTransformation[F, A, R, B2, R2] = new StreamTransformation[F, A, R, B2, R2] {
    override type S = StreamTransformation.this.S

    override val initialState: S = StreamTransformation.this.initialState

    override def processItems[S2](state: S, state2: S2, items: NonEmptyVector[A])(f2: (S2, NonEmptyVector[B2]) => F[S2])(implicit monadInstance: Monad[F]): F[(S, S2)] =
      StreamTransformation.this.processItems(state, state2, items) { (s2, bItems) =>
        bItems.toVector.flatMap(f) match {
          case Vector() => s2.point[F]
          case head +: tail => f2(s2, NonEmptyVector(head, tail))
        }
      }

    override def processResult[S2](state: S, state2: S2, result: R)(f2: (S2, NonEmptyVector[B2]) => F[S2])(implicit monadInstance: Monad[F]): F[(R2, S2)] =
      StreamTransformation.this.processResult(state, state2, result) { (s2, bItems) =>
        bItems.toVector.flatMap(f) match {
          case Vector() => s2.point[F]
          case head +: tail => f2(s2, NonEmptyVector(head, tail))
        }
      }

  }


  final def mapResult[R3](f: R2 => R3): StreamTransformation[F, A, R, B, R3] = new StreamTransformation[F, A, R, B, R3] {
    override type S = StreamTransformation.this.S

    override val initialState: S = StreamTransformation.this.initialState

    override def processItems[S2](state: S, state2: S2, items: NonEmptyVector[A])(f2: (S2, NonEmptyVector[B]) => F[S2])(implicit monadInstance: Monad[F]): F[(S, S2)] =
      StreamTransformation.this.processItems(state, state2, items)(f2)

    override def processResult[S2](state: S, state2: S2, result: R)(f2: (S2, NonEmptyVector[B]) => F[S2])(implicit monadInstance: Monad[F]): F[(R3, S2)] =
      StreamTransformation.this.processResult(state, state2, result)(f2).map { case (result, state2) => (f(result), state2) }

  }

}

object StreamTransformation {
  trait Single[F[_], A, R, B, R2] extends StreamTransformation[F, A, R, B, R2] {
    protected def processItem[S2](state: S, state2: S2, item: A)(f: (S2, NonEmptyVector[B]) => F[S2])(implicit monadInstance: Monad[F]): F[(S, S2)]

    override def processItems[S2](state: S, state2: S2, items: NonEmptyVector[A])(f: (S2, NonEmptyVector[B]) => F[S2])(implicit monadInstance: Monad[F]): F[(S, S2)] =
      items.foldLeftM((state, state2)) {
        case ((state, state2), item) =>
          processItem(state, state2, item)(f)
      }
  }
}
