package dev.argon.stream.builder

import cats.Monad

trait Builder[F[_], S] extends Monad[F] {
  def append(value: S): F[Unit]
}
