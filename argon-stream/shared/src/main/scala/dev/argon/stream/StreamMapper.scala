package dev.argon.stream
import zio.{Chunk, IO, NonEmptyChunk, ZIO}

final class StreamMapper[-R, +E, -A, X, +B](f: A => ZIO[R, E, B]) extends StreamTransformation[R, E, A, X, B, X] {
  override type TransformState = Unit

  override def start: ZIO[R, E, Unit] = IO.unit

  override def consume(state: Unit, values: NonEmptyChunk[A]): ZIO[R, E, (Unit, Chunk[B])] =
    values.toChunk.mapM(f).map { ((), _) }

  override def finish(state: Unit, value: X): ZIO[R, E, (Chunk[B], X)] =
    IO.succeed((Chunk.empty, value))

}
