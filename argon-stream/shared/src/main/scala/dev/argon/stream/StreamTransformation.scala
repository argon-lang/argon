package dev.argon.stream

import zio.{Chunk, IO, NonEmptyChunk, ZIO}

trait StreamTransformation[-R, +E, -A, -X, +B, +Y] {

  type TransformState
  def start: ZIO[R, E, TransformState]
  def consume(state: TransformState, values: NonEmptyChunk[A]): ZIO[R, E, (TransformState, Chunk[B])]
  def finish(state: TransformState, value: X): ZIO[R, E, (Chunk[B], Y)]

  final def mapStream[C](f: B => C): StreamTransformation[R, E, A, X, C, Y] =
    andThen(new StreamMapper[R, E, B, Y, C](b => IO.succeed(f(b))))

  final def mapStreamM[R2 <: R, E2 >: E, C](f: B => ZIO[R2, E2, C]): StreamTransformation[R2, E2, A, X, C, Y] =
    andThen(new StreamMapper[R2, E2, B, Y, C](f))

  final def collect[C](f: PartialFunction[B, C]): StreamTransformation[R, E, A, X, C, Y] =
    andThen(new StreamCollector[R, E, B, Y, C](f.andThen(IO.succeed(_))))

  final def mapResult[Z](f: Y => Z): StreamTransformation[R, E, A, X, B, Z] =
    andThen(new StreamResultMapper[R, E, B, Y, Z](y => IO.succeed(f(y))))

  final def mapResultM[R2 <: R, E2 >: E, Z](f: Y => ZIO[R2, E2, Z]): StreamTransformation[R2, E2, A, X, B, Z] =
    andThen(new StreamResultMapper[R2, E2, B, Y, Z](f))

  final def unit: StreamTransformation[R, E, A, X, B, Unit] =
    mapResult(_ => ())

  final def mapError[E2](f: E => E2): StreamTransformation[R, E2, A, X, B, Y] =
    new StreamTransformation[R, E2, A, X, B, Y] {
      override type TransformState = StreamTransformation.this.TransformState

      override def start: ZIO[R, E2, TransformState] =
        StreamTransformation.this.start.mapError(f)

      override def consume(state: TransformState, values: NonEmptyChunk[A]): ZIO[R, E2, (TransformState, Chunk[B])] =
        StreamTransformation.this.consume(state, values).mapError(f)

      override def finish(state: TransformState, value: X): ZIO[R, E2, (Chunk[B], Y)] =
        StreamTransformation.this.finish(state, value).mapError(f)
    }



  final def andThen[R2 <: R, E2 >: E, B2 >: B, Y2 >: Y, C, Z](other: StreamTransformation[R2, E2, B, Y, C, Z]): StreamTransformation[R2, E2, A, X, C, Z] =
    new StreamTransformation[R2, E2, A, X, C, Z] {
      override type TransformState = (StreamTransformation.this.TransformState, other.TransformState)

      override def start: ZIO[R2, E2, TransformState] = for {
        s1 <- StreamTransformation.this.start
        s2 <- other.start
      } yield (s1, s2)

      override def consume(state: TransformState, values: NonEmptyChunk[A]): ZIO[R2, E2, (TransformState, Chunk[C])] =
        StreamTransformation.this.consume(state._1, values).flatMap { case (s1, chunkB) =>
          (NonEmptyChunk.fromChunk(chunkB) match {
            case Some(chunkB) => other.consume(state._2, chunkB)
            case None => IO.succeed((state._2, Chunk.empty))
          }).map { case (s2, chunkC) =>
            ((s1, s2), chunkC)
          }
        }


      override def finish(state: TransformState, value: X): ZIO[R2, E2, (Chunk[C], Z)] =
        StreamTransformation.this.finish(state._1, value).flatMap { case (chunkB, y) =>
          (NonEmptyChunk.fromChunk(chunkB) match {
            case Some(chunkB) => other.consume(state._2, chunkB)
            case None => IO.succeed((state._2, Chunk.empty))
          }).flatMap { case (s2, chunkC1) =>
            other.finish(s2, y).map { case (chunkC2, z) =>
              (chunkC1 ++ chunkC2, z)
            }
          }
        }
    }

}
