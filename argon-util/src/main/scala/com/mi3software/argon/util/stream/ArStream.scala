package com.mi3software.argon.util.stream

import scalaz._
import Scalaz._

trait ArStream[F[_], A, R] {

  def foldChunksM[B, R2](start: B)(resultHandler: (B, R) => R2)(f: (B, Vector[A]) => F[B])(implicit monadInstance: Monad[F]): F[R2]
  def foldChunks[B, R2](start: B)(resultHandler: (B, R) => R2)(f: (B, Vector[A]) => B)(implicit monadInstance: Monad[F]): F[R2] =
    foldChunksM(start)(resultHandler) { (b, items) => f(b, items).pure[F] }

  def foldLeftM[B, R2](start: B)(resultHandler: (B, R) => R2)(f: (B, A) => F[B])(implicit monadInstance: Monad[F]): F[R2] =
    foldChunksM(start)(resultHandler) { (b, items) => items.foldLeftM(b)(f) }

  def foldLeft[B, R2](start: B)(resultHandler: (B, R) => R2)(f: (B, A) => B)(implicit monadInstance: Monad[F]): F[R2] =
    foldChunks(start)(resultHandler) { (b, items) => items.foldLeft(b)(f) }

  def chunkSize(size: Int)(implicit monadInstance: Monad[F]): ArStream[F, A, R] = chunkLimit(size).chunkBuffer(size)

  def chunkBuffer(size: Int)(implicit monadInstance: Monad[F]): ArStream[F, A, R] = new ArStream[F, A, R] {
    override def foldChunksM[B, R2](start: B)(resultHandler: (B, R) => R2)(f: (B, Vector[A]) => F[B])(implicit monadInstance: Monad[F]): F[R2] =
      ArStream.this.foldChunksM((start, Vector.empty[A])) {
        case ((b, Vector()), result) => resultHandler(b, result).point[F]
        case ((b, acc), result) => f(b, acc).map { b2 => resultHandler(b2, result) }
      } {
        case ((b, Vector()), items) if items.size < size => (b, items).point[F]
        case ((b, Vector()), items) => f(b, items).map { b2 => (b2, Vector.empty) }
        case ((b, acc), items) if acc.size + items.size < size => (b, acc ++ items).point[F]
        case ((b, acc), items) => f(b, acc ++ items).map { b2 => (b2, Vector.empty) }
      }.flatMap(identity)
  }

  def chunkLimit(size: Int)(implicit monadInstance: Monad[F]): ArStream[F, A, R] = new ArStream[F, A, R] {
    override def foldChunksM[B, R2](start: B)(resultHandler: (B, R) => R2)(f: (B, Vector[A]) => F[B])(implicit monadInstance: Monad[F]): F[R2] =
      ArStream.this.foldChunksM(start)(resultHandler) { (b, items) =>
          def foldPartial(state: B, items: Vector[A]): F[B] =
            if(items.isEmpty)
              state.point[F]
            else if(items.size > size) {
              val (curr, next) = items.splitAt(size)
              f(state, curr).flatMap { newState => foldPartial(newState, next) }
            }
            else
              f(state, items)

          foldPartial(b, items)
      }
  }



  def mapItems[B](f: A => B)(implicit monadInstance: Monad[F]): ArStream[F, B, R] = new ArStream[F, B, R] {
    override def foldChunksM[C, R2](start: C)(resultHandler: (C, R) => R2)(f2: (C, Vector[B]) => F[C])(implicit monadInstance: Monad[F]): F[R2] =
      ArStream.this.foldChunksM(start)(resultHandler) { (c, items) => f2(c, items.map(f)) }
  }

  def mapResult[R2](f: R => R2)(implicit monadInstance: Monad[F]): ArStream[F, A, R2] = new ArStream[F, A, R2] {
    override def foldChunksM[B, R3](start: B)(resultHandler: (B, R2) => R3)(f2: (B, Vector[A]) => F[B])(implicit monadInstance: Monad[F]): F[R3] =
      ArStream.this.foldChunksM(start) { (b, r) => resultHandler(b, f(r)) } (f2)
  }

  def transformM[B, R2, S](start: S)(resultHandler: (S, R) => F[(R2, Vector[B])])(f: (S, A) => F[(S, Vector[B])]): ArStream[F, B, R2] = new ArStream[F, B, R2] {
    override def foldChunksM[C, R3](startC: C)(resultHandlerC: (C, R2) => R3)(fC: (C, Vector[B]) => F[C])(implicit monadInstance: Monad[F]): F[R3] =
      ArStream.this.foldLeftM((start, startC)) {
        case ((s, c), r) =>
          resultHandler(s, r).flatMap {
            case (r2, Vector()) => resultHandlerC(c, r2).point[F]
            case (r2, items) => fC(c, items).map { c2 => resultHandlerC(c2, r2) }
          }
      } { case ((s, c), a) =>
        f(s, a).flatMap {
          case (s2, Vector()) =>
            (s2, c).point[F]

          case (s2, b) =>
            fC(c, b).map { c2 =>
              (s2, c2)
            }
        }
      }.flatMap(identity)
  }

  def transform[B, R2, S](start: S)(resultHandler: (S, R) => (R2, Vector[B]))(f: (S, A) => (S, Vector[B]))(implicit monadInstance: Monad[F]): ArStream[F, B, R2] =
    transformM(start) { (s, r) => resultHandler(s, r).point[F] } { (s, a) => f(s, a).point[F] }

}

object ArStream {

  def fromVector[F[_], A, R](items: Vector[A], result: R): ArStream[F, A, R] = new ArStream[F, A, R] {
    override def foldChunksM[B, R2](start: B)(resultHandler: (B, R) => R2)(f: (B, Vector[A]) => F[B])(implicit monadInstance: Monad[F]): F[R2] =
      if(items.isEmpty)
        resultHandler(start, result).point[F]
      else
        f(start, items).map { state => resultHandler(state, result) }
  }

}
