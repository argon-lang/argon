package dev.argon.parser.impl

import cats._
import cats.data._
import cats.implicits._
import dev.argon.stream.builder._
import dev.argon.util._
import zio.{IO, ZIO}


object Characterizer {

  private def toCodePoints[R, E](chars: Source[R, E, Char, Unit]): Source[R, E, Int, Unit] = new Source[R, E, Int, Unit] {
    override def foreach[R1 <: R, E1 >: E](consume: Int => ZIO[R1, E1, Unit]): ZIO[R1, E1, Unit] =
      chars.foldLeftM[R1, E1, Option[Char]](Option.empty[Char]) {
        case (Some(prevCh), ch) => consume(Character.toCodePoint(prevCh, ch)).map { _ => Option.empty[Char] }
        case (None, ch) if Character.isHighSurrogate(ch) => IO.succeed(Some(ch))
        case (None, ch) => consume(ch.toInt).as { Option.empty[Char] }
      }.flatMap {
        case (Some(ch), _) => consume(ch.toInt)
        case (None, _) => IO.unit
      }
  }

  private def toGraphemes[R, E](codepoints: Source[R, E, Int, Unit]): Source[R, E, String, Unit] = new Source[R, E, String, Unit] {
    override def foreach[R1 <: R, E1 >: E](consume: String => ZIO[R1, E1, Unit]): ZIO[R1, E1, Unit] =
      codepoints.foldLeftM[R1, E1, Option[String]](Option.empty[String]) {
        case (Some(str), cp) if isCombiningChar(cp) => IO.succeed(Some(str + codePointToString(cp)))
        case (Some(str), cp) => consume(str).as { Some(codePointToString(cp)) }
        case (None, cp) => IO.succeed(Some(codePointToString(cp)))
      }.flatMap {
        case (Some(str), _) => consume(str)
        case (None, _) => IO.unit
      }
  }

  private def withSource[R, E](graphemes: Source[R, E, String, Unit]): Source[R, E, WithSource[String], FilePosition] = new Source[R, E, WithSource[String], FilePosition] {


    override def foreach[R1 <: R, E1 >: E](consume: WithSource[String] => ZIO[R1, E1, Unit]): ZIO[R1, E1, FilePosition] =
      graphemes.foldLeftM[R1, E1, FilePosition](FilePosition(1, 1)) { (pos, item) =>
        val nextPos =
          if(item === "\n")
            FilePosition(pos.line + 1, 1)
          else
            pos.copy(position = pos.position + 1)

        val newItem = WithSource(item, SourceLocation(pos, nextPos))

        consume(newItem).as { nextPos }
      }.map { case (pos, _) => pos }
  }

  private def isCombiningChar(cp: Int): Boolean =
    Character.getType(cp) match {
      case Character.COMBINING_SPACING_MARK | Character.ENCLOSING_MARK | Character.NON_SPACING_MARK => true
      case _ => false
    }

  private def codePointToString(cp: Int): String =
    new String(Character.toChars(cp))


  def characterize[R, E](chars: Source[R, E, Char, Unit]): Source[R, E, WithSource[String], FilePosition] =
    chars.into(toCodePoints[R, E]).into(toGraphemes[R, E]).into(withSource[R, E])

}
