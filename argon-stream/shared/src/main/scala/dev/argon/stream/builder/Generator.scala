package dev.argon.stream.builder

import cats._
import cats.implicits._

trait Generator[A] {
  def create[F[_]](implicit builder: Builder[F, A]): F[Unit]
}

object Generator {

  implicit def generatorIterInstance[F[_]: Monad]: Iter[F, Generator] = new Iter[F, Generator] {
    override def foldLeftM[A, S](data: Generator[A])(value: S)(f: (S, A) => F[S]): F[S] =
      data.create(StepBuilder.stepBuilderBuilderInstance[F, A]).flatMap { sb =>
        StepBuilder.stepBuilderIterInstance[F].foldLeftM(sb)(value)(f)
      }

  }

}
