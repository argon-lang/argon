package dev.argon.stream.builder

import cats._
import cats.arrow.FunctionK
import cats.data.NonEmptyVector
import cats.implicits._

trait Generator[F[_], A, X] {
  def create[G[_]](implicit genEffect: GenEffect[F, G], builder: Builder[G, A]): G[X]

  def into[B, Y](f: Generator[F, A, X] => Generator[F, B, Y]): Generator[F, B, Y] = f(this)


  def buffer(count: Int)(implicit monad: Monad[F]): Generator[F, NonEmptyVector[A], X] = new Generator[F, NonEmptyVector[A], X] {
    override def create[G[_]](implicit genEffect: GenEffect[F, G], builder: Builder[G, NonEmptyVector[A]]): G[X] =
      Iter[G, Generator[G, ?, ?], X].foldLeftM(Generator.this.translateEffect[G])(Vector.empty[A]) { (prev, a) =>
        val newBuff = prev :+ a
        if(prev.size + 1 >= count)
          builder.append(NonEmptyVector.fromVectorUnsafe(newBuff)).map { _ => Vector.empty }
        else
          builder.pure(newBuff)
      }.flatMap {
        case (h +: t, x) => builder.append(NonEmptyVector(h, t)).map { _ => x }
        case (Vector(), x) => builder.pure(x)
      }
  }

  def collect[B](f: PartialFunction[A, B])(implicit monad: Monad[F]): Generator[F, B, X] =
    new Generator[F, B, X] {
      override def create[G[_]](implicit genEffect: GenEffect[F, G], builder: Builder[G, B]): G[X] =
        Iter[G, Generator[G, ?, ?], X].foreach(Generator.this.translateEffect[G]) { a =>
          f.lift(a) match {
            case Some(value) => builder.append(value)
            case None => builder.pure(())
          }
        }
    }

  def mapResult[Y](f: X => Y): Generator[F, A, Y] =
    new Generator[F, A, Y] {
      override def create[G[_]](implicit genEffect: GenEffect[F, G], builder: Builder[G, A]): G[Y] =
        Generator.this.create.map(f)
    }


  def translateEffect[G[_]](implicit genEffect: GenEffect[F, G]): Generator[G, A, X] = new Generator[G, A, X] {
    override def create[H[_]](implicit genEffect2: GenEffect[G, H], builder: Builder[H, A]): H[X] = {
      implicit val composedEffect = GenEffect.genEffectComposed[F, G, H]
      Generator.this.create[H]
    }
  }

  def flatMap[B, Y](f: A => Generator[F, B, Unit])(implicit monad: Monad[F]): Generator[F, B, X] = new Generator[F, B, X] {
    override def create[G[_]](implicit genEffect: GenEffect[F, G], builder: Builder[G, B]): G[X] =
      Iter[G, Generator[G, ?, ?], X].foreach(Generator.this.translateEffect[G]) { a =>
        Iter[G, Generator[G, ?, ?], Unit].foreach(f(a).translateEffect[G])(builder.append)
      }
  }
}

object Generator {



  implicit def generatorIterInstance[F[_]: Monad, X0]: Iter[F, Generator[F, ?, ?], X0] = new Iter[F, Generator[F, ?, ?], X0] {
    override def foldLeftM[A, X <: X0, S](data: Generator[F, A, X])(value: S)(f: (S, A) => F[S]): F[(S, X)] =
      data.create(StepBuilder.stepBuilderGenEffectInstance[F, A], StepBuilder.stepBuilderBuilderInstance[F, A]).flatMap { sb =>
        StepBuilder.stepBuilderIterInstance[F, X].foldLeftM(sb)(value)(f)
      }

  }

}
