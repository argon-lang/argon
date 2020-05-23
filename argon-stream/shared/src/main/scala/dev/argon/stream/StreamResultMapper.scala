package dev.argon.stream
import zio.{Chunk, IO, NonEmptyChunk, ZIO}

final class StreamResultMapper[-R, +E, A, -X, +Y](f: X => ZIO[R, E, Y]) extends StreamTransformation[R, E, A, X, A, Y] {
  override type TransformState = Unit

  override def start: ZIO[R, E, Unit] = IO.unit

  override def consume(state: Unit, values: NonEmptyChunk[A]): ZIO[R, E, (Unit, Chunk[A])] =
    IO.succeed(((), values.toChunk))

  override def finish(state: Unit, value: X): ZIO[R, E, (Chunk[A], Y)] =
    f(value).map { (Chunk.empty, _) }
}
