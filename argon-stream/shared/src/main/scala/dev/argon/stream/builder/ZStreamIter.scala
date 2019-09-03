package dev.argon.stream.builder

import cats.{Monad, ~>}
import zio.{IO, ZIO}
import zio.stream.ZStream
import cats._
import cats.implicits._

object ZStreamIter {

  implicit def zStreamIterInstance[R, E, X0]: Iter[ZIO[R, E, ?], Lambda[(A, X) => (ZStream[R, E, A], X)], X0] =
    new Iter[ZIO[R, E, ?], Lambda[(A, X) => (ZStream[R, E, A], X)], X0] {
      override def foldLeftM[A, X <: X0, S](data: (ZStream[R, E, A], X))(state: S)(f: (S, A) => ZIO[R, E, S]): ZIO[R, E, (S, X)] =
        data._1.fold(state)(_ => true)(f).map { (_, data._2) }
    }

}
