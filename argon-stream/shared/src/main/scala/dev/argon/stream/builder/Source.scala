package dev.argon.stream.builder

import cats._
import cats.arrow.FunctionK
import cats.data.{NonEmptyVector, StateT}
import cats.implicits._

trait Source[F[_], A, X] {

  protected implicit val monadF: Monad[F]

  final def generate[G[_]: Monad](sink: Sink[G, A])(implicit genEffect: GenEffect[F, G]): G[X] =
    genEffect.statefully(
      stateful = new GenEffect.StatefulHandler[F, G, Unit, A, X, X] {

        override def apply[H[_] : Monad, S2](state: Unit, state2: S2)(consume: (Unit, S2, A) => H[(Unit, S2)])(implicit genEffect: GenEffect[F, H]): H[(Unit, S2, X)] =
          foldLeftG[H, S2](state2) { (s, a) => consume((), s, a).map { case (_, s) => s } }.map { case (s, x) => ((), s, x) }

        override def initialState: Unit = ()

        override def foldState(state: Unit, value: A): G[Unit] = sink.consume(value)

        override def consumeState(state: Unit, result: X): G[X] = result.pure[G]

      },
      stateless = generateImpl(sink)
    )

  protected def generateImpl[G[_]: Monad](sink: Sink[G, A])(implicit genEffect: GenEffect[F, G]): G[X]

  final def foldLeftG[G[_]: Monad, S](state: S)(f: (S, A) => G[S])(implicit genEffect: GenEffect[F, G]): G[(S, X)] =
    genEffect.ifSame(
      sameHandler = unlift => foldLeftM(state) { (s, a) => unlift(f(s, a)) },
      notSameHandler =
        genEffect.statefully(
          stateful = new GenEffect.StatefulHandler[F, G, S, A, X, (S, X)] {
            override def apply[H[_] : Monad, S2](state: S, state2: S2)(consume: (S, S2, A) => H[(S, S2)])(implicit genEffect: GenEffect[F, H]): H[(S, S2, X)] =
              foldLeftGImpl((state, state2)) { case ((state, state2), a) =>
                consume(state, state2, a)
              }
                .map { case ((state, state2), x) => (state, state2, x) }

            override def initialState: S = state

            override def foldState(state: S, value: A): G[S] = f(state, value)

            override def consumeState(state: S, result: X): G[(S, X)] = (state, result).pure[G]
          },
          stateless = foldLeftGImpl(state)(f)
        )
    )

  protected def foldLeftGImpl[G[_]: Monad, S](state: S)(f: (S, A) => G[S])(implicit genEffect: GenEffect[F, G]): G[(S, X)] =
    generateImpl[StateT[G, S, *]](new Sink[StateT[G, S, *], A] {
      override def consume(value: A): StateT[G, S, Unit] =
        StateT { state => f(state, value).map { (_, ()) } }
    }).run(state)


  def foldLeftM[S](state: S)(f: (S, A) => F[S]): F[(S, X)] =
    generateImpl[StateT[F, S, *]](new Sink[StateT[F, S, *], A] {
      override def consume(value: A): StateT[F, S, Unit] =
        StateT { state => f(state, value).map { (_, ()) } }
    }).run(state)

  def foreachG[G[_]: Monad](f: A => G[Unit])(implicit genEffect: GenEffect[F, G]): G[X] =
    genEffect.ifSame(
      sameHandler = unlift => foreach { a => unlift(f(a)) },
      notSameHandler =
        generate(new Sink[G, A] {
          override def consume(value: A): G[Unit] =
            f(value)
        })
    )


  def foreach(f: A => F[Unit]): F[X] =
    generate(new Sink[F, A] {
      override def consume(value: A): F[Unit] =
        f(value)
    })

  def drain(sink: Sink[F, A]): F[X] =
    generate(sink)

  final def into[B, Y](f: Source[F, A, X] => Source[F, B, Y]): Source[F, B, Y] = f(this)

  def flatMap[B](f: A => Source[F, B, Unit]): Source[F, B, X] = new Source[F, B, X] {

    override protected val monadF: Monad[F] = Source.this.monadF


    override protected def generateImpl[G[_] : Monad](sink: Sink[G, B])(implicit genEffect: GenEffect[F, G]): G[X] =
      Source.this.generate(new Sink[G, A] {
        override def consume(value: A): G[Unit] =
          f(value).generate(sink)
      })

    override def foldLeftM[S](state: S)(g: (S, B) => F[S]): F[(S, X)] =
      Source.this.foldLeftM(state) { (state, a) =>
        monadF.map(f(a).foldLeftM(state)(g)) { case (state, _) => state }
      }

    override def foreach(g: B => F[Unit]): F[X] =
      Source.this.foreach { a =>
        f(a).foreach(g)
      }
  }

  def map[B](f: A => B): Source[F, B, X] = new Source[F, B, X] {
    override protected val monadF: Monad[F] = Source.this.monadF


    override protected def generateImpl[G[_] : Monad](sink: Sink[G, B])(implicit genEffect: GenEffect[F, G]): G[X] =
      Source.this.generate(new Sink[G, A] {
        override def consume(value: A): G[Unit] =
          sink.consume(f(value))
      })

    override def foldLeftM[S](state: S)(g: (S, B) => F[S]): F[(S, X)] =
      Source.this.foldLeftM(state) { (state, a) => g(state, f(a)) }

    override def foreach(g: B => F[Unit]): F[X] =
      Source.this.foreach(g compose f)
  }

  def collect[B](f: PartialFunction[A, B]): Source[F, B, X] = new Source[F, B, X] {
    override protected val monadF: Monad[F] = Source.this.monadF


    override protected def generateImpl[G[_] : Monad](sink: Sink[G, B])(implicit genEffect: GenEffect[F, G]): G[X] =
      Source.this.generate(new Sink[G, A] {
        override def consume(value: A): G[Unit] =
          f.lift(value).fold(().pure[G])(sink.consume)
      })

    override def foldLeftM[S](state: S)(g: (S, B) => F[S]): F[(S, X)] =
      Source.this.foldLeftM(state) { (state, a) => g(state, f(a)) }

    override def foreach(g: B => F[Unit]): F[X] =
      Source.this.foreach(g compose f)
  }


  def mapResult[Y](f: X => Y): Source[F, A, Y] = new Source[F, A, Y] {
    override protected implicit val monadF: Monad[F] = Source.this.monadF


    override protected def generateImpl[G[_] : Monad](sink: Sink[G, A])(implicit genEffect: GenEffect[F, G]): G[Y] =
      Source.this.generate(sink).map(f)

    override def foldLeftGImpl[G[_] : Monad, S](state: S)(g: (S, A) => G[S])(implicit genEffect: GenEffect[F, G]): G[(S, Y)] =
      Source.this.foldLeftG(state)(g).map { case (state, x) => (state, f(x)) }

    override def foldLeftM[S](state: S)(g: (S, A) => F[S]): F[(S, Y)] =
      Source.this.foldLeftM(state)(g).map { case (state, x) => (state, f(x)) }

    override def foreachG[G[_] : Monad](g: A => G[Unit])(implicit genEffect: GenEffect[F, G]): G[Y] =
      Source.this.foreachG(g).map(f)

    override def foreach(g: A => F[Unit]): F[Y] =
      Source.this.foreach(g).map(f)
  }

  def bufferVector(count: Int): Source[F, NonEmptyVector[A], X] = new Source[F, NonEmptyVector[A], X] {
    override protected implicit val monadF: Monad[F] = Source.this.monadF


    override protected def generateImpl[G[_] : Monad](sink: Sink[G, NonEmptyVector[A]])(implicit genEffect: GenEffect[F, G]): G[X] =
      Source.this.generate(new Sink[StateT[G, Vector[A], *], A] {
        override def consume(value: A): StateT[G, Vector[A], Unit] =
          StateT { acc =>
            if(acc.size + 1 >= count)
              sink.consume(NonEmptyVector.fromVectorUnsafe(acc :+ value)).map { _ => (Vector.empty[A], ()) }
            else
              (acc :+ value, ()).pure[G]
          }
      })
        .run(Vector.empty)
        .flatMap {
          case (acc, x) =>
            NonEmptyVector.fromVector(acc) match {
              case Some(rest) => sink.consume(rest).map { _ => x }
              case None => x.pure[G]
            }
        }

  }

}
