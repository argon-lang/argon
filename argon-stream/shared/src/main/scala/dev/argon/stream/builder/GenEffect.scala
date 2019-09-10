package dev.argon.stream.builder

import cats._
import cats.arrow.FunctionK
import cats.implicits._
import cats.data.StateT

trait GenEffect[F[_], G[_]] {

  def ifSame[A]
  (
    sameHandler: (G ~> F) => F[A],
    notSameHandler: => G[A]
  ): G[A] = notSameHandler

  def liftF[A](fa: F[A]): G[A]
  def useG[A, B, S](state: S)(resource: UseFunc[F, A, S])(g: (S, A) => G[(S, B)]): G[(S, B)]
}

object GenEffect {

  implicit def genEffectIdentity[F[_]]: GenEffect[F, F] = new GenEffect[F, F] {

    override def ifSame[A](sameHandler: F ~> F => F[A], notSameHandler: => F[A]): F[A] =
      sameHandler(FunctionK.id)

    override def liftF[A](fa: F[A]): F[A] = fa

    override def useG[A, B, S](state: S)(resource: UseFunc[F, A, S])(g: (S, A) => F[(S, B)]): F[(S, B)] =
      resource.use(state)(g)
  }

  implicit def genEffectStateTSecond[F[_]: Monad, G[_]: Monad, S](implicit genEffect: GenEffect[F, G]): GenEffect[F, StateT[G, S, *]] = new GenEffect[F, StateT[G, S, *]] {
    override def liftF[B](fb: F[B]): StateT[G, S, B] =
      StateT.liftF(genEffect.liftF(fb))


    override def useG[A, B, S2](state2: S2)(resource: UseFunc[F, A, S2])(g: (S2, A) => StateT[G, S, (S2, B)]): StateT[G, S, (S2, B)] =
      StateT { state =>
        genEffect.useG(state2)(resource) { (state2, a) =>
          g(state2, a).run(state)
            .map { case (state, (state2, c)) => (state2, (state, c)) }
        }
          .map { case (state2, (state, b)) => (state, (state2, b)) }
      }

  }

  implicit def genEffectStateTBoth[F[_]: Monad, G[_]: Monad, S](implicit genEffect: GenEffect[F, G]): GenEffect[StateT[F, S, *], StateT[G, S, *]] =
    new GenEffect[StateT[F, S, *], StateT[G, S, *]] {
      override def liftF[A](fa: StateT[F, S, A]): StateT[G, S, A] =
        StateT { state =>
          genEffect.liftF(fa.run(state))
        }


      override def useG[A, B, S2](state2: S2)(resource: UseFunc[StateT[F, S, *], A, S2])(g: (S2, A) => StateT[G, S, (S2, B)]): StateT[G, S, (S2, B)] =
        StateT { state =>
          genEffect.useG((state, state2))(new UseFunc[F, A, (S, S2)] {
            override def use[C](state: (S, S2))(f: ((S, S2), A) => F[((S, S2), C)]): F[((S, S2), C)] =
              resource.use(state._2) { (state2, a) =>
                StateT[F, S, (S2, C)] { state =>
                  f((state, state2), a)
                    .map { case ((state, state2), c) => (state, (state2, c)) }
                }
              }
                .run(state._1)
                .map { case (state, (state2, c)) => ((state, state2), c) }

          }) { case ((state, state2), a) =>
            g(state2, a).run(state)
              .map { case (state, (state2, c)) => ((state, state2), c) }
          }
            .map { case ((state, state2), c) => (state, (state2, c)) }
        }

    }

}
