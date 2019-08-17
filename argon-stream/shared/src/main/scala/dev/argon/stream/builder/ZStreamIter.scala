package dev.argon.stream.builder

import cats.{Monad, ~>}
import zio.ZIO
import zio.stream.ZStream

object ZStreamIter {

  implicit def zStreamIterInstance[R, E, X0]: Iter[ZIO[R, E, ?], Lambda[(A, X) => (ZStream[R, E, A], X)], X0] =
    new Iter[ZIO[R, E, ?], Lambda[(A, X) => (ZStream[R, E, A], X)], X0] {
      override def foldLeftM[G[_] : Monad, A, X <: X0, S](convert: ZIO[R, E, ?] ~> G)(data: (ZStream[R, E, A], X))(state: S)(f: (S, A) => G[S]): G[(S, X)] =
        ???
    }

}
