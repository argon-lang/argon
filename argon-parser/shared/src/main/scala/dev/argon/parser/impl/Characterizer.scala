package dev.argon.parser.impl

import cats.implicits._
import dev.argon.stream.StreamTransformation
import dev.argon.util._
import zio.{Chunk, IO, NonEmptyChunk, UIO, ZIO}


object Characterizer {

  private def toCodePoints: StreamTransformation[Any, Nothing, Char, Unit, Int, Unit] =
    new StreamTransformation[Any, Nothing, Char, Unit, Int, Unit] {
      override type TransformState = Option[Char]

      override def start: UIO[Option[Char]] = IO.none

      override def consume(state: Option[Char], values: NonEmptyChunk[Char]): UIO[(Option[Char], Chunk[Int])] = {

        def convert(state: Option[Char], values: Chunk[Char], acc: Chunk[Int]): (Option[Char], Chunk[Int]) =
          (state, values) match {
            case (Some(prevCh), ch +: tail) => convert(None, tail, acc :+ Character.toCodePoint(prevCh, ch))
            case (None, ch +: tail) if Character.isHighSurrogate(ch) => convert(Some(ch), tail, acc)
            case (None, ch +: tail) => convert(None, tail, acc :+ ch.toInt)
            case (_, _) => (state, acc)
          }

        IO.succeed(convert(state, values.toChunk, Chunk.empty))
      }


      override def finish(state: Option[Char], value: Unit): UIO[(Chunk[Int], Unit)] =
        IO.succeed((Chunk.fromIterable(state.map { _.toInt }.toList), ()))
    }

  private def toGraphemes: StreamTransformation[Any, Nothing, Int, Unit, String, Unit] =
    new StreamTransformation[Any, Nothing, Int, Unit, String, Unit] {
      override type TransformState = Option[String]

      override def start: UIO[Option[String]] = IO.none

      override def consume(state: Option[String], values: NonEmptyChunk[Int]): UIO[(Option[String], Chunk[String])] = {

        def convert(state: Option[String], values: Chunk[Int], acc: Chunk[String]): (Option[String], Chunk[String]) =
          (state, values) match {
            case (Some(str), cp +: tail) if isCombiningChar(cp) => convert(Some(str + codePointToString(cp)), tail, acc)
            case (Some(str), cp +: tail) => convert(Some(codePointToString(cp)), tail, acc :+ str)
            case (None, cp +: tail) => convert(Some(codePointToString(cp)), tail, acc)
            case (_, _) => (state, acc)
          }

        IO.succeed(convert(state, values.toChunk, Chunk.empty))
      }

      override def finish(state: Option[String], value: Unit): ZIO[Any, Nothing, (Chunk[String], Unit)] =
        IO.succeed((Chunk.fromIterable(state.toList), ()))
    }


  private def withSource: StreamTransformation[Any, Nothing, String, Unit, WithSource[String], FilePosition] =
    new StreamTransformation[Any, Nothing, String, Unit, WithSource[String], FilePosition] {
      override type TransformState = FilePosition

      override def start: ZIO[Any, Nothing, FilePosition] = IO.succeed(FilePosition(1, 1))

      override def consume(pos: FilePosition, values: NonEmptyChunk[String]): ZIO[Any, Nothing, (FilePosition, Chunk[WithSource[String]])] =
        IO.succeed(
          values.toChunk.mapAccum(pos) { (pos, item) =>
            val nextPos =
              if(item === "\n")
                FilePosition(pos.line + 1, 1)
              else
                pos.copy(position = pos.position + 1)

            val newItem = WithSource(item, SourceLocation(pos, nextPos))
            (nextPos, newItem)
          }
        )


      override def finish(pos: FilePosition, value: Unit): ZIO[Any, Nothing, (Chunk[WithSource[String]], FilePosition)] =
        IO.succeed((Chunk.empty, pos))
    }

  private def isCombiningChar(cp: Int): Boolean =
    Character.getType(cp) match {
      case Character.COMBINING_SPACING_MARK | Character.ENCLOSING_MARK | Character.NON_SPACING_MARK => true
      case _ => false
    }

  private def codePointToString(cp: Int): String =
    new String(Character.toChars(cp))


  def characterize: StreamTransformation[Any, Nothing, Char, Unit, WithSource[String], FilePosition] =
    toCodePoints.andThen(toGraphemes).andThen(withSource)

}
