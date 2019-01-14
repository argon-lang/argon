package com.mi3software.argon.util.stream

import scalaz._
import Scalaz._

trait ArStream[F[_], A] {

  def foldChunksM[B](start: B)(f: (B, Vector[A]) => F[B])(implicit monadInstance: Monad[F]): F[B]
  def foldChunks[B](start: B)(f: (B, Vector[A]) => B)(implicit monadInstance: Monad[F]): F[B] = foldChunksM(start) { (b, items) => f(b, items).pure[F] }

  def foldLeftM[B](start: B)(f: (B, A) => F[B])(implicit monadInstance: Monad[F]): F[B] =
    foldChunksM(start) { (b, items) => items.foldLeftM(b)(f) }

  def foldLeft[B](start: B)(f: (B, A) => B)(implicit monadInstance: Monad[F]): F[B] =
    foldChunks(start) { (b, items) => items.foldLeft(b)(f) }

  def chunkSize(size: Int)(implicit monadInstance: Monad[F]): ArStream[F, A] = chunkLimit(size).chunkBuffer(size)

  def chunkBuffer(size: Int)(implicit monadInstance: Monad[F]): ArStream[F, A] = new ArStream[F, A] {
    override def foldChunksM[B](start: B)(f: (B, Vector[A]) => F[B])(implicit monadInstance: Monad[F]): F[B] =
      ArStream.this.foldChunksM((start, Vector.empty[A])) {
        case ((b, Vector()), items) if items.size < size => (b, items).point[F]
        case ((b, Vector()), items) => f(b, items).map { b2 => (b2, Vector.empty) }
        case ((b, acc), items) if acc.size + items.size < size => (b, acc ++ items).point[F]
        case ((b, acc), items) => f(b, acc ++ items).map { b2 => (b2, Vector.empty) }
      }.flatMap {
        case (b, Vector()) => b.point[F]
        case (b, acc) => f(b, acc)
      }
  }

  def chunkLimit(size: Int)(implicit monadInstance: Monad[F]): ArStream[F, A] = new ArStream[F, A] {
    override def foldChunksM[B](start: B)(f: (B, Vector[A]) => F[B])(implicit monadInstance: Monad[F]): F[B] =
      ArStream.this.foldChunksM(start) { (b, items) =>
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

  def map[B](f: A => B)(implicit monadInstance: Monad[F]): ArStream[F, B] = new ArStream[F, B] {
    override def foldChunksM[C](start: C)(f2: (C, Vector[B]) => F[C])(implicit monadInstance: Monad[F]): F[C] =
      ArStream.this.foldChunksM(start) { (c, items) => f2(c, items.map(f)) }
  }

  def flatMap[B](f: A => ArStream[F, B])(implicit monadInstance: Monad[F]): ArStream[F, B] = new ArStream[F, B] {
    override def foldChunksM[C](start: C)(f2: (C, Vector[B]) => F[C])(implicit monadInstance: Monad[F]): F[C] =
      ArStream.this.foldLeftM(start) { (b, a) => f(a).foldChunksM(b)(f2) }
  }

  def translate[G[_]](tl: F ~> G)(implicit monadInstance: Monad[F]): ArStream[G, A] = new ArStream[G, A] {
    override def foldChunksM[B](start: B)(f: (B, Vector[A]) => G[B])(implicit monadInstance2: Monad[G]): G[B] =
      tl(ArStream.this.foldChunks(start.point[G]) { (acc, items) => acc.flatMap { b => f(b, items) } })
        .flatMap(identity)
  }

}

object ArStream {

  def fromVector[F[_], A](items: Vector[A]): ArStream[F, A] = new ArStream[F, A] {
    override def foldChunksM[B](start: B)(f: (B, Vector[A]) => F[B])(implicit monadInstance: Monad[F]): F[B] =
      if(items.isEmpty)
        start.point[F]
      else
        f(start, items)
  }

}
