package dev.argon.stream.builder

import cats._
import cats.arrow.FunctionK
import cats.data.NonEmptyVector
import cats.implicits._

trait Generator[F[_], A, X] {
  def create[G[_]](convert: F ~> G)(implicit builder: Builder[G, A]): G[X]

  def into[B, Y](f: Generator[F, A, X] => Generator[F, B, Y]): Generator[F, B, Y] = f(this)


  def buffer(count: Int)(implicit monad: Monad[F]): Generator[F, NonEmptyVector[A], X] = new Generator[F, NonEmptyVector[A], X] {
    override def create[G[_]](convert: F ~> G)(implicit builder: Builder[G, NonEmptyVector[A]]): G[X] =
      Iter[F, Generator[F, ?, ?], X].foldLeftM(convert)(Generator.this)(Vector.empty[A]) { (prev, a) =>
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
      override def create[G[_]](convert: F ~> G)(implicit builder: Builder[G, B]): G[X] =
        Iter[F, Generator[F, ?, ?], X].foreach(convert)(Generator.this) { a =>
          f.lift(a) match {
            case Some(value) => builder.append(value)
            case None => builder.pure(())
          }
        }
    }

  def mapResult[Y](f: X => Y): Generator[F, A, Y] =
    new Generator[F, A, Y] {
      override def create[G[_]](convert: F ~> G)(implicit builder: Builder[G, A]): G[Y] =
        Generator.this.create(convert).map(f)
    }

  def translateEffect[G[_]](convert: F ~> G): Generator[G, A, X] = new Generator[G, A, X] {
    override def create[H[_]](convert2: G ~> H)(implicit builder: Builder[H, A]): H[X] =
      Generator.this.create(convert.andThen(convert2))
  }

  def flatMap[B, Y](f: A => Generator[F, B, Unit])(implicit monad: Monad[F]): Generator[F, B, X] = new Generator[F, B, X] {
    override def create[G[_]](convert: F ~> G)(implicit builder: Builder[G, B]): G[X] =
      Iter[F, Generator[F, ?, ?], X].foreach(convert)(Generator.this) { a =>
        Iter[F, Generator[F, ?, ?], Unit].foreach(convert)(f(a))(builder.append)
      }
  }
}

object Generator {



  implicit def generatorIterInstance[F[_]: Monad, X0]: Iter[F, Generator[F, ?, ?], X0] = new Iter[F, Generator[F, ?, ?], X0] {
    override def foldLeftM[G[_]: Monad, A, X <: X0, S](convert: F ~> G)(data: Generator[F, A, X])(value: S)(f: (S, A) => G[S]): G[(S, X)] =
      convert(data.create(StepBuilder.lift[F, A])(StepBuilder.stepBuilderBuilderInstance[F, A])).flatMap { sb =>
        StepBuilder.stepBuilderIterInstance[F, X].foldLeftM(convert)(sb)(value)(f)
      }

  }

}
