package dev.argon.stream.builder

import cats.Monad
import zio.{IO, Queue, ZIO}
import zio.stream.{Take, ZStream}

trait SourceIO[R, E, A, X] extends Source[ZIO[R, E, *], A, X] {
  override protected implicit val monadF: Monad[ZIO[R, E, *]] = zio.interop.catz.monadErrorInstance

  def toZStream: ZStream[R, E, A] =
    ZStream.flatten(
      ZStream.fromEffect(
        for {
          queue <- Queue.bounded[Take[E, A]](1)

          sink = new Sink[ZIO[R, E, *], A] {
            override def consume(value: A): ZIO[R, E, Unit] =
              queue.offer(Take.Value(value)).unit
          }

          _ <- generate(sink).foldCauseM(
            failure = cause => queue.offer(Take.Fail(cause)).unit,
            success = _ => queue.offer(Take.End).unit
          ).fork

        } yield ZStream.fromQueue(queue).unTake
      )
    )

}

object SourceIO {

  def fromSource[R, E, A, X](source: Source[ZIO[R, E, *], A, X]): SourceIO[R, E, A, X] =
    source match {
      case source: SourceIO[R, E, A, X] => source
      case _ =>
        new SourceIO[R, E, A, X] {
          override def generate[G[_] : Monad](sink: Sink[G, A])(implicit genEffect: GenEffect[ZIO[R, E, *], G]): G[X] =
            source.generate(sink)

          override def foldLeftG[G[_] : Monad, S](state: S)(f: (S, A) => G[S])(implicit genEffect: GenEffect[ZIO[R, E, *], G]): G[(S, X)] =
            source.foldLeftG(state)(f)

          override def foldLeftM[S](state: S)(f: (S, A) => ZIO[R, E, S]): ZIO[R, E, (S, X)] =
            source.foldLeftM(state)(f)

          override def foreachG[G[_] : Monad](f: A => G[Unit])(implicit genEffect: GenEffect[ZIO[R, E, *], G]): G[X] =
            source.foreachG(f)

          override def foreach(f: A => ZIO[R, E, Unit]): ZIO[R, E, X] = source.foreach(f)
        }
    }

}
