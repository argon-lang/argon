package dev.argon.stream.builder

import cats._
import cats.data.NonEmptyVector
import cats.implicits._
import zio._
import zio.stream.ZStream
import zio.stream.ZStream.Pull

import Function.const

final class ZStreamSource[-R, +E, +A](stream: ZStream[R, E, A]) extends Source[R, E, A, Unit] {

  override def toZStream: ZStream[R, E, A] = stream


  override def foreach[R1 <: R, E1 >: E](f: A => ZIO[R1, E1, Unit]): ZIO[R1, E1, Unit] =
    stream.foreach(f)

  override def foldLeftM[R1 <: R, E1 >: E, S](state: S)(f: (S, A) => ZIO[R1, E1, S]): ZIO[R1, E1, (S, Unit)] =
    stream.foldM[R1, E1, A, S](state)(f).map { s => (s, ()) }

  override def foldLeft[S](state: S)(f: (S, A) => S): ZIO[R, E, (S, Unit)] =
    stream.fold(state)(f).map { s => (s, ()) }

  override def map[B](f: A => B): Source[R, E, B, Unit] =
    new ZStreamSource(stream.map(f))

  override def collect[B](f: PartialFunction[A, B]): Source[R, E, B, Unit] =
    new ZStreamSource(stream.collect(f))


}
