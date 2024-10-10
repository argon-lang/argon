package dev.argon.grammar

import dev.argon.util.*
import dev.argon.util.async.*
import zio.stream.ZChannel
import zio.{Chunk, IO, NonEmptyChunk, UIO, ZIO}

object Characterizer {

  private def toCodePoints: ZChannel[Any, Nothing, Chunk[Char], Any, Nothing, Chunk[Int], Any] =
    ZChannelUtil.mapAccumOption[Char, Int, Option[Char]](None) {
      case (Some(prevCh), ch) => (None, Some(Character.toCodePoint(prevCh, ch)))
      case (None, ch) if Character.isHighSurrogate(ch) => (Some(ch), None)
      case (None, ch) => (None, Some(ch.toInt))
    }

  private def toGraphemes: ZChannel[Any, Nothing, Chunk[Int], Any, Nothing, Chunk[String], Any] =
    ZChannelUtil.mapAccumOption[Int, String, Option[String]](None) {
      case (Some(str), cp) if isCombiningChar(cp) => (Some(str + codePointToString(cp)), None)
      case (Some(str), cp) => (Some(codePointToString(cp)), Some(str))
      case (None, cp) => (Some(codePointToString(cp)), None)
    }

  private def withSource(fileName: Option[String]): ZChannel[Any, Nothing, Chunk[String], Any, Nothing, Chunk[WithSource[String]], FilePosition] =
    ZChannelUtil.mapAccum[String, WithSource[String], FilePosition](FilePosition(1, 1)) { (pos, item) =>
      val nextPos =
        if item == "\n" then FilePosition(pos.line + 1, 1)
        else pos.copy(position = pos.position + 1)

      val newItem = WithLocation(item, Location(fileName, pos, nextPos))
      (nextPos, newItem)
    }

  private def isCombiningChar(cp: Int): Boolean =
    Character.getType(cp) match {
      case Character.COMBINING_SPACING_MARK | Character.ENCLOSING_MARK | Character.NON_SPACING_MARK => true
      case _ => false
    }

  private def codePointToString(cp: Int): String = new String(Character.toChars(cp))

  def characterize(fileName: Option[String]): ZChannel[Any, Nothing, Chunk[Char], Any, Nothing, Chunk[WithSource[String]], FilePosition] =
    toCodePoints.pipeTo(toGraphemes).pipeTo(withSource(fileName))

}
