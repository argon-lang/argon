package com.mi3software.argon.parser.impl

import com.mi3software.argon.parser.SyntaxError
import com.mi3software.argon.util.{EitherTFlattener, FilePosition, SourceLocation, WithSource}

import scalaz._
import Scalaz._
import com.thoughtworks.each.Monadic._

object Characterizer {

  sealed trait InvalidUnicode
  private final case class InvalidSurrogate(ch: Char) extends InvalidUnicode
  private final case class UnexpectedCombingChar(cp: Int) extends InvalidUnicode

  private def toCodePoints[M[_] : Monad](chars: StreamT[M, Char]): StreamT[EitherT[M, InvalidUnicode, ?], Int] =
    StreamT[EitherT[M, InvalidUnicode, ?], Int](
      monadic[EitherT[M, InvalidUnicode, ?]] {

        type StepType = StreamT.Step[Int, StreamT[EitherT[M, InvalidUnicode, ?], Int]]

        EitherT(chars.uncons.map(\/.right[InvalidUnicode, Option[(Char, StreamT[M, Char])]])).each match {
          case Some((ch, tail)) if Character.isHighSurrogate(ch) =>
            EitherT(tail.uncons.map(\/.right[InvalidUnicode, Option[(Char, StreamT[M, Char])]])).each match {
              case Some((low, tail2)) if Character.isLowSurrogate(low) =>
                StreamT.Yield(Character.toCodePoint(ch, low), toCodePoints(tail2))

              case Some((low, _)) =>
                EitherT[M, InvalidUnicode, StepType](Monad[M].point(-\/(InvalidSurrogate(low)))).each

              case None =>
                EitherT[M, InvalidUnicode, StepType](Monad[M].point(-\/(InvalidSurrogate(ch)))).each
            }
          case Some((ch, _)) if Character.isSurrogate(ch) =>
            EitherT[M, InvalidUnicode, StepType](Monad[M].point(-\/(InvalidSurrogate(ch)))).each

          case Some((ch, tail))=>
            StreamT.Yield(ch.toInt, toCodePoints(tail))

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

  private def appendCombingChars[M[_] : Monad](s: String, cps: StreamT[M, Int]): StreamT[EitherT[M, InvalidUnicode, ?], String] =
    StreamT[EitherT[M, InvalidUnicode, ?], String](
      monadic[EitherT[M, InvalidUnicode, ?]] {
        EitherT(cps.uncons.map(\/.right[InvalidUnicode, Option[(Int, StreamT[M, Int])]])).each match {
          case Some((cp, tail)) if !isBaseCharacterType(cp) =>
            StreamT.Skip(appendCombingChars(s + new String(Character.toChars(cp)), tail))

          case _ =>
            StreamT.Yield(s, getGraphemes(cps))
        }
      }
    )

  private def getGraphemes[M[_] : Monad](cps: StreamT[M, Int]): StreamT[EitherT[M, InvalidUnicode, ?], String] =
    StreamT[EitherT[M, InvalidUnicode, ?], String](
      monadic[EitherT[M, InvalidUnicode, ?]] {
        type StepType = StreamT.Step[String, StreamT[EitherT[M, InvalidUnicode, ?], String]]

        EitherT(cps.uncons.map(\/.right[InvalidUnicode, Option[(Int, StreamT[M, Int])]])).each match {
          case Some((cp, tail)) if isBaseCharacterType(cp) =>
            StreamT.Skip(appendCombingChars(new String(Character.toChars(cp)), tail))

          case Some((cp, tail)) =>
            EitherT[M, InvalidUnicode, StepType](Monad[M].point(-\/(UnexpectedCombingChar(cp)))).each

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

  private def withLocation[M[_] : Monad](chs: StreamT[EitherT[M, InvalidUnicode, ?], String]): StreamT[EitherT[M, SyntaxError, ?], WithSource[String]] = {
    def impl(pos: FilePosition, chs: StreamT[EitherT[M, InvalidUnicode, ?], String]): StreamT[EitherT[M, SyntaxError, ?], WithSource[String]] =
      StreamT[EitherT[M, SyntaxError, ?], WithSource[String]](
        EitherT[M, SyntaxError, StreamT.Step[WithSource[String], StreamT[EitherT[M, SyntaxError, ?], WithSource[String]]]](
          monadic[M] {
            chs.uncons.run.each match {
              case -\/(InvalidSurrogate(ch)) =>
                -\/(SyntaxError.InvalidSurrogatePairs(ch, posToLoc(pos)))

              case -\/(UnexpectedCombingChar(cp)) =>
                -\/(SyntaxError.UnexpectedCombingCharacter(cp, posToLoc(pos)))

              case \/-(Some((head, tail))) =>
                \/-(StreamT.Yield(
                  WithSource(head, posToLoc(pos)),
                  impl(nextPosition(head, pos), tail)
                ))

              case \/-(None) =>
                \/-(StreamT.Done)

            }
          }
        )
      )

    impl(FilePosition(1, 1), chs)
  }

  def characterize[M[_] : Monad](chars: StreamT[M, Char]): StreamT[EitherT[M, SyntaxError, ?], WithSource[String]] = {

    implicit val level1UnicodeInst = EitherT.eitherTMonad[M, InvalidUnicode]
    implicit val level2UnicodeInst = EitherT.eitherTMonad[EitherT[M, InvalidUnicode, ?], InvalidUnicode]

    val cps = toCodePoints(chars)
    val chs = getGraphemes[EitherT[M, InvalidUnicode, ?]](cps).trans[EitherT[M, InvalidUnicode, ?]](new EitherTFlattener[M, InvalidUnicode])

    withLocation(chs)
  }

}
