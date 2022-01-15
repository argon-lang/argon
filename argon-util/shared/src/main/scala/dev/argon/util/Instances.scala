package dev.argon.util

import zio.*
import zio.stream.*

given [L]: Monad[[R] =>> Either[L, R]] with
  override def flatMap[A, B](fa: Either[L, A])(f: A => Either[L, B]): Either[L, B] = fa.flatMap(f)
  override def pure[A](a: A): Either[L, A] = Right(a)

  override def map[A, B](fa: Either[L, A])(f: A => B): Either[L, B] = fa.map(f)
end given

given Traverse[List] with Monad[List] with
  override def flatMap[A, B](fa: List[A])(f: A => List[B]): List[B] = fa.flatMap(f)
  override def pure[A](a: A): List[A] = List(a)

  override def traverse[F[+_]: Applicative, A, B](ca: List[A])(f: A => F[B]): F[List[B]] =
    ca match {
      case _: Nil.type => Applicative[F].pure(Nil)
      case h :: t => Applicative[F].map2(f(h), traverse(t)(f)) { (h2, t2) => h2 :: t2 }
    }

  def foldLeftM[F[+_]: Monad, S, A](ca: List[A])(s: S)(f: (S, A) => F[S]): F[S] =
    ca match {
      case _: Nil.type => Applicative[F].pure(s)
      case h :: t => f(s, h).flatMap { s2 => foldLeftM(t)(s2)(f) }
    }

  def foldLeft[S, A](ca: List[A])(s: S)(f: (S, A) => S): S = ca.foldLeft(s)(f)

  override def map[A, B](fa: List[A])(f: A => B): List[B] = fa.map(f)
end given

given Traverse[Chunk] with Monad[Chunk] with
  override def flatMap[A, B](fa: Chunk[A])(f: A => Chunk[B]): Chunk[B] = fa.flatMap(f)
  override def pure[A](a: A): Chunk[A] = Chunk(a)

  override def traverse[F[+_]: Applicative, A, B](ca: Chunk[A])(f: A => F[B]): F[Chunk[B]] =
    ca match {
      case h +: t => Applicative[F].map2(f(h), traverse(t)(f)) { (h2, t2) => h2 +: t2 }
      case _ => Applicative[F].pure(Chunk.empty)
    }

  def foldLeftM[F[+_]: Monad, S, A](ca: Chunk[A])(s: S)(f: (S, A) => F[S]): F[S] =
    ca match {
      case h +: t => f(s, h).flatMap { s2 => foldLeftM(t)(s2)(f) }
      case _ => Applicative[F].pure(s)
    }

  def foldLeft[S, A](ca: Chunk[A])(s: S)(f: (S, A) => S): S = ca.foldLeft(s)(f)

  override def map[A, B](fa: Chunk[A])(f: A => B): Chunk[B] = fa.map(f)
end given

given TraverseNonEmpty[NonEmptyChunk] with Monad[NonEmptyChunk] with

  override def flatMap[A, B](fa: NonEmptyChunk[A])(f: A => NonEmptyChunk[B]): NonEmptyChunk[B] =
    fa.flatMap(f).asInstanceOf[NonEmptyChunk[B]]

  override def pure[A](a: A): NonEmptyChunk[A] = NonEmptyChunk(a)

  override def traverse[F[+_]: Applicative, A, B](ca: NonEmptyChunk[A])(f: A => F[B]): F[NonEmptyChunk[B]] =
    Applicative[F].map2(f(ca.head), summon[Traverse[Chunk]].traverse(ca.tail)(f)) { (h2, t2) =>
      NonEmptyChunk(h2, t2*)
    }

  def foldLeftM[F[+_]: Monad, S, A](ca: NonEmptyChunk[A])(s: S)(f: (S, A) => F[S]): F[S] =
    f(s, ca.head).flatMap { s2 => summon[Traverse[Chunk]].foldLeftM(ca.tail)(s2)(f) }

  def foldLeft[S, A](ca: NonEmptyChunk[A])(s: S)(f: (S, A) => S): S = ca.foldLeft(s)(f)

  def reduceLeftM[F[+_]: Monad, A](ca: NonEmptyChunk[A])(f: (A, A) => F[A]): F[A] =
    summon[Traverse[Chunk]].foldLeftM(ca.tail)(ca.head)(f)

  def reduceLeft[A](ca: NonEmptyChunk[A])(f: (A, A) => A): A = ca.reduceLeft(f)

  override def map[A, B](fa: NonEmptyChunk[A])(f: A => B): NonEmptyChunk[B] = fa.map(f).asInstanceOf[NonEmptyChunk[B]]
end given

given Traverse[Vector] with Monad[Vector] with
  override def flatMap[A, B](fa: Vector[A])(f: A => Vector[B]): Vector[B] = fa.flatMap(f)
  override def pure[A](a: A): Vector[A] = Vector(a)

  override def traverse[F[+_]: Applicative, A, B](ca: Vector[A])(f: A => F[B]): F[Vector[B]] =
    ca match {
      case h +: t => Applicative[F].map2(f(h), traverse(t)(f)) { (h2, t2) => h2 +: t2 }
      case _ => Applicative[F].pure(Vector.empty)
    }

  def foldLeftM[F[+_]: Monad, S, A](ca: Vector[A])(s: S)(f: (S, A) => F[S]): F[S] =
    ca match {
      case h +: t => f(s, h).flatMap { s2 => foldLeftM(t)(s2)(f) }
      case _ => Applicative[F].pure(s)
    }

  def foldLeft[S, A](ca: Vector[A])(s: S)(f: (S, A) => S): S = ca.foldLeft(s)(f)

  override def map[A, B](fa: Vector[A])(f: A => B): Vector[B] = fa.map(f)
end given

given [R, E]: Monad[[A] =>> ZIO[R, E, A]] with
  override def flatMap[A, B](fa: ZIO[R, E, A])(f: A => ZIO[R, E, B]): ZIO[R, E, B] = fa.flatMap(f)
  override def pure[A](a: A): ZIO[R, E, A] = ZIO.succeed(a)
  override def map[A, B](fa: ZIO[R, E, A])(f: A => B): ZIO[R, E, B] = fa.map(f)
end given

given [R, E]: Monad[[A] =>> ZStream[R, E, A]] with
  override def flatMap[A, B](fa: ZStream[R, E, A])(f: A => ZStream[R, E, B]): ZStream[R, E, B] = fa.flatMap(f)
  override def pure[A](a: A): ZStream[R, E, A] = ZStream.succeed(a)
  override def map[A, B](fa: ZStream[R, E, A])(f: A => B): ZStream[R, E, B] = fa.map(f)
end given
