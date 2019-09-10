package dev.argon.stream.builder

import cats.Monad

trait Sink[F[_], A] {
  def consume(value: A): F[Unit]
}
