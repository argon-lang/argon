package com.mi3software.argon.util.stream

import scalaz._
import Scalaz._
import com.mi3software.argon.util.NonEmptyVector

trait ArStream[F[_], A, R] {

  def foldChunksM[B, R2](start: B)(resultHandler: (B, R) => R2)(f: (B, NonEmptyVector[A]) => F[B])(implicit monadInstance: Monad[F]): F[R2]
  def foldChunks[B, R2](start: B)(resultHandler: (B, R) => R2)(f: (B, NonEmptyVector[A]) => B)(implicit monadInstance: Monad[F]): F[R2] =
    foldChunksM(start)(resultHandler) { (b, items) => f(b, items).pure[F] }

  def foldLeftM[B, R2](start: B)(resultHandler: (B, R) => R2)(f: (B, A) => F[B])(implicit monadInstance: Monad[F]): F[R2] =
    foldChunksM(start)(resultHandler) { (b, items) => items.foldLeftM(b)(f) }

  def foldLeft[B, R2](start: B)(resultHandler: (B, R) => R2)(f: (B, A) => B)(implicit monadInstance: Monad[F]): F[R2] =
    foldChunks(start)(resultHandler) { (b, items) => items.foldLeft(b)(f) }

  def chunkSize(size: Int)(implicit monadInstance: Monad[F]): ArStream[F, A, R] = chunkLimit(size).chunkBuffer(size)

  def chunkBuffer(size: Int)(implicit monadInstance: Monad[F]): ArStream[F, A, R] = new ArStream[F, A, R] {
    override def foldChunksM[B, R2](start: B)(resultHandler: (B, R) => R2)(f: (B, NonEmptyVector[A]) => F[B])(implicit monadInstance: Monad[F]): F[R2] =
      ArStream.this.foldChunksM((start, Vector.empty[A])) {
        case ((b, Vector()), result) => resultHandler(b, result).point[F]
        case ((b, head +: tail), result) => f(b, NonEmptyVector(head, tail)).map { b2 => resultHandler(b2, result) }
      } {
        case ((b, Vector()), items) if items.size < size => (b, items.toVector).point[F]
        case ((b, Vector()), items) => f(b, items).map { b2 => (b2, Vector.empty) }
        case ((b, acc), items) if acc.size + items.size < size => (b, acc ++ items.toVector).point[F]
        case ((b, acc), items) => f(b, items.prepend(acc)).map { b2 => (b2, Vector.empty) }
      }.flatMap(identity)
  }

  def chunkLimit(size: Int)(implicit monadInstance: Monad[F]): ArStream[F, A, R] = new ArStream[F, A, R] {
    override def foldChunksM[B, R2](start: B)(resultHandler: (B, R) => R2)(f: (B, NonEmptyVector[A]) => F[B])(implicit monadInstance: Monad[F]): F[R2] =
      ArStream.this.foldChunksM(start)(resultHandler) { (b, items) =>
          def foldPartial(state: B, items: NonEmptyVector[A]): F[B] =
            if(items.size > size) {
              items.splitAt(size) match {
                case (curr, head +: tail) =>
                  val next = NonEmptyVector(head, tail)
                  f(state, curr).flatMap { newState => foldPartial(newState, next) }

                case (curr, Vector()) =>
                  f(state, curr)
              }
            }
            else
              f(state, items)

          foldPartial(b, items)
      }
  }



  def mapItems[B](f: A => B)(implicit monadInstance: Monad[F]): ArStream[F, B, R] = new ArStream[F, B, R] {
    override def foldChunksM[C, R2](start: C)(resultHandler: (C, R) => R2)(f2: (C, NonEmptyVector[B]) => F[C])(implicit monadInstance: Monad[F]): F[R2] =
      ArStream.this.foldChunksM(start)(resultHandler) { (c, items) => f2(c, items.map(f)) }
  }

  def mapResult[R2](f: R => R2)(implicit monadInstance: Monad[F]): ArStream[F, A, R2] = new ArStream[F, A, R2] {
    override def foldChunksM[B, R3](start: B)(resultHandler: (B, R2) => R3)(f2: (B, NonEmptyVector[A]) => F[B])(implicit monadInstance: Monad[F]): F[R3] =
      ArStream.this.foldChunksM(start) { (b, r) => resultHandler(b, f(r)) } (f2)
  }

  def transformWith[B, R2](transformation: StreamTransformation[F, A, R, B, R2])(implicit monadInstance: Monad[F]): ArStream[F, B, R2] = new ArStream[F, B, R2] {
    override def foldChunksM[C, R3](startC: C)(resultHandlerC: (C, R2) => R3)(fC: (C, NonEmptyVector[B]) => F[C])(implicit monadInstance: Monad[F]): F[R3] =
      ArStream.this.foldChunksM((transformation.initialState, startC)) {
        case ((s, c), r) => transformation.processResult(s, c, r)(fC)
      } {
        case ((s, c), a) => transformation.processItems(s, c, a)(fC)
      }.flatMap(_.map { case (r2, c) => resultHandlerC(c, r2) })
  }

  def merge[B, R2: Monoid](mergeResult: (R2, R) => R2)(f: A => ArStream[F, B, R2]): ArStream[F, B, R2] = new ArStream[F, B, R2] {
    override def foldChunksM[C, R3](start: C)(resultHandler: (C, R2) => R3)(f2: (C, NonEmptyVector[B]) => F[C])(implicit monadInstance: Monad[F]): F[R3] =
      ArStream.this.foldLeftM((start, Monoid[R2].zero)) { case ((c, r2), r) => resultHandler(c, mergeResult(r2, r)) } {
        case ((c, r2), a) =>
          f(a).foldChunksM(c)(Tuple2.apply)(f2)
      }
  }

  def toVector(implicit monadInstance: Monad[F]): F[Vector[A]] =
    foldLeft(Vector.empty[A]) { (acc, _) => acc } { (acc, a) => acc :+ a }

}

object ArStream {

  def fromVector[F[_], A, R](items: Vector[A], result: R): ArStream[F, A, R] = new ArStream[F, A, R] {
    override def foldChunksM[B, R2](start: B)(resultHandler: (B, R) => R2)(f: (B, NonEmptyVector[A]) => F[B])(implicit monadInstance: Monad[F]): F[R2] =
      items match {
        case Vector() => resultHandler(start, result).point[F]
        case head +: tail => f(start, NonEmptyVector(head, tail)).map { state => resultHandler(state, result) }
      }
  }

  def wrapEffect[F[_], A, R](stream: F[ArStream[F, A, R]]): ArStream[F, A, R] = new ArStream[F, A, R] {
    override def foldChunksM[B, R2](start: B)(resultHandler: (B, R) => R2)(f: (B, NonEmptyVector[A]) => F[B])(implicit monadInstance: Monad[F]): F[R2] =
      stream.flatMap { _.foldChunksM(start)(resultHandler)(f) }
  }

}
