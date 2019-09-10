package dev.argon.stream.builder

trait UseFunc[F[_], A, S] {
  def use[B](state: S)(f: (S, A) => F[(S, B)]): F[(S, B)]
}
