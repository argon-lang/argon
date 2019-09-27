package dev.argon.stream.builder

import cats._
import cats.implicits._
import zio._
import zio.stream.ZStream
import zio.stream.ZStream.Pull

import Function.const

final case class ZStreamSource[R, E, A](stream: ZStream[R, E, A]) extends SourceIO[R, E, A, Unit] {


  override def toZStream: ZStream[R, E, A] = stream

  override def generateImpl[G[_] : Monad](sink: Sink[G, A])(implicit genEffect: GenEffect[ZIO[R, E, *], G]): G[Unit] =
    genEffect.useG(())(new UseFunc[ZIO[R, E, *], ZStream.Pull[R, E, A], Unit] {

      override def use[B](state: Unit)(f: (Unit, Pull[R, E, A]) => ZIO[R, E, (Unit, B)]): ZIO[R, E, (Unit, B)] =
        stream.process.use { pull => f((), pull) }
    }) { (_, pull) =>

      def readStream: G[(Unit, Unit)] =
        genEffect.liftF(
          pull.map(Some.apply).catchAll {
            case Some(e) =>  IO.fail(e)
            case None => IO.succeed(None)
          }
        )
          .flatMap {
            case Some(value) => sink.consume(value) *> readStream
            case None => ((), ()).pure[G]
          }

      readStream
    }
      .map { _ => () }

  override def foldLeftM[S](state: S)(f: (S, A) => ZIO[R, E, S]): ZIO[R, E, (S, Unit)] =
    stream.foldM(state)(f).map { (_, ()) }

  override def foreach(f: A => ZIO[R, E, Unit]): ZIO[R, E, Unit] =
    stream.foreach(f)

  override def map[B](f: A => B): Source[ZIO[R, E, *], B, Unit] =
    ZStreamSource(stream.map(f))

}
