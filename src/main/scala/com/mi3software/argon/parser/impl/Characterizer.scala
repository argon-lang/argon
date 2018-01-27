package com.mi3software.argon.parser.impl

import com.mi3software.argon.parser.SyntaxError
import com.mi3software.argon.util.{FilePosition, SourceLocation, WithSource}

import scalaz._
import Scalaz._

import com.thoughtworks.each.Monadic._

object Characterizer {

  private sealed trait InvalidUnicode
  private final case class InvalidSurrogate(ch: Char) extends InvalidUnicode
  private final case class UnexpectedCombingChar(cp: Int) extends InvalidUnicode

  private def toCodePoints[M[_] : Monad](chars: StreamT[M, Char]): StreamT[M, InvalidUnicode \/ Int] =
    StreamT[M, InvalidUnicode \/ Int](
      monadic[M] {
        chars.uncons.each match {
          case Some((ch, tail)) if Character.isHighSurrogate(ch) =>
            tail.uncons.each match {
              case Some((low, tail2)) if Character.isLowSurrogate(low) =>
                StreamT.Yield(\/-(Character.toCodePoint(ch, low)), toCodePoints(tail2))

              case Some((low, _)) =>
                StreamT.Yield(-\/(InvalidSurrogate(low)), StreamT.empty)

              case None =>
                StreamT.Yield(-\/(InvalidSurrogate(ch)), StreamT.empty)
            }
          case Some((ch, _)) if Character.isSurrogate(ch) =>
            StreamT.Yield(-\/(InvalidSurrogate(ch)), StreamT.empty)

          case Some((ch, tail))=>
            StreamT.Yield(\/-(ch.toInt), toCodePoints(tail))

          case None =>
            StreamT.Done
        }
      }
    )

  private def isBaseCharacterType(cp: Int): Boolean =
    Character.getType(cp) match {
      case Character.NON_SPACING_MARK | Character.COMBINING_SPACING_MARK | Character.ENCLOSING_MARK =>
        false

      case _ =>
        true
    }

  private def appendCombingChars[M[_] : Monad](s: String, cps: StreamT[M, InvalidUnicode \/ Int]): StreamT[M, InvalidUnicode \/ String] =
    StreamT[M, InvalidUnicode \/ String](
      monadic[M] {
        cps.uncons.each match {
          case Some((-\/(err), _)) => StreamT.Yield(-\/(err), StreamT.empty)
          case Some((\/-(cp), tail)) if !isBaseCharacterType(cp) =>
            StreamT.Skip(appendCombingChars(s + new String(Character.toChars(cp)), tail))

          case _ =>
            StreamT.Yield(\/-(s), getGraphemes(cps))
        }
      }
    )

  private def getGraphemes[M[_] : Monad](cps: StreamT[M, InvalidUnicode \/ Int]): StreamT[M, InvalidUnicode \/ String] =
    StreamT[M, InvalidUnicode \/ String](
      monadic[M] {
        cps.uncons.each match {
          case Some((-\/(err), _)) => StreamT.Yield(-\/(err), StreamT.empty)
          case Some((\/-(cp), tail)) if isBaseCharacterType(cp) =>
            StreamT.Skip(appendCombingChars(new String(Character.toChars(cp)), tail))

          case Some((\/-(cp), tail)) =>
            StreamT.Yield(-\/(UnexpectedCombingChar(cp)), StreamT.empty)

          case None =>
            StreamT.Done
        }
      }
    )

  private def nextPosition(ch: String, pos: FilePosition): FilePosition =
    if(ch === "\n")
      FilePosition(pos.line + 1, 1)
    else
      FilePosition(pos.line, pos.position + 1)

  private def posToLoc(pos: FilePosition): SourceLocation =
    SourceLocation(pos, FilePosition(pos.line, pos.position + 1))

  private def withLocation[M[_] : Monad](chs: StreamT[M, InvalidUnicode \/ String]): StreamT[M, SyntaxError \/ WithSource[String]] = {
    def impl(pos: FilePosition, chs: StreamT[M, InvalidUnicode \/ String]): StreamT[M, SyntaxError \/ WithSource[String]] =
      StreamT[M, SyntaxError \/ WithSource[String]](
        monadic[M] {
          chs.uncons.each match {
            case Some((-\/(InvalidSurrogate(ch)), _)) =>
              StreamT.Yield(-\/(SyntaxError.InvalidSurrogatePairs(ch, posToLoc(pos))), StreamT.empty)

            case Some((-\/(UnexpectedCombingChar(cp)), _)) =>
              StreamT.Yield(-\/(SyntaxError.UnexpectedCombingCharacter(cp, posToLoc(pos))), StreamT.empty)

            case Some((\/-(head), tail)) =>
              StreamT.Yield(
                \/-(WithSource(head, posToLoc(pos))),
                impl(nextPosition(head, pos), tail)
              )

            case None =>
              StreamT.Done
          }
        }
      )

    impl(FilePosition(1, 1), chs)
  }

  def characterize[M[_] : Monad](chars: StreamT[M, Char]): StreamT[M, SyntaxError \/ WithSource[String]] =
    withLocation(getGraphemes(toCodePoints(chars)))

}
