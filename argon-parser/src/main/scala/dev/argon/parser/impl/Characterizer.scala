package dev.argon.parser.impl

import dev.argon.util.*
import zio.{Chunk, IO, NonEmptyChunk, UIO, ZIO}
import zio.stream.ZChannel

object Characterizer {

  private def toCodePoints[E]: ZChannel[Any, E, Chunk[Char], Any, E, Chunk[Int], Any] =
    ZChannelUtil.mapAccumOption[E, Char, Int, Option[Char]](None) {
      case (Some(prevCh), ch) => (None, Some(Character.toCodePoint(prevCh, ch)))
      case (None, ch) if Character.isHighSurrogate(ch) => (Some(ch), None)
      case (None, ch) => (None, Some(ch.toInt))
    }

  private def toGraphemes[E]: ZChannel[Any, E, Chunk[Int], Any, E, Chunk[String], Any] =
    ZChannelUtil.mapAccumOption[E, Int, String, Option[String]](None) {
      case (Some(str), cp) if isCombiningChar(cp) => (Some(str + codePointToString(cp)), None)
      case (Some(str), cp) => (Some(codePointToString(cp)), Some(str))
      case (None, cp) => (Some(codePointToString(cp)), None)
    }

  private def withSource[E](fileName: Option[String]): ZChannel[Any, E, Chunk[String], Any, E, Chunk[WithSource[String]], FilePosition] =
    ZChannelUtil.mapAccum[E, String, WithSource[String], FilePosition](FilePosition(1, 1)) { (pos, item) =>
      val nextPos =
        if item == "\n" then FilePosition(pos.line + 1, 1)
        else pos.copy(position = pos.position + 1)

      val newItem = WithSource(item, SourceLocation(fileName, pos, nextPos))
      (nextPos, newItem)
    }

  private def isCombiningChar(cp: Int): Boolean =
    Character.getType(cp) match {
      case Character.COMBINING_SPACING_MARK | Character.ENCLOSING_MARK | Character.NON_SPACING_MARK => true
      case _ => false
    }

  private def codePointToString(cp: Int): String = new String(Character.toChars(cp))

  def characterize[E](fileName: Option[String]): ZChannel[Any, E, Chunk[Char], Any, E, Chunk[WithSource[String]], FilePosition] =
    toCodePoints.pipeTo(toGraphemes).pipeTo(withSource(fileName))

}
