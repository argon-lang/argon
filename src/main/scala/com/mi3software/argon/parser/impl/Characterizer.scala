package com.mi3software.argon.parser.impl

import com.mi3software.argon.parser.SyntaxError
import com.mi3software.argon.util._

import scalaz._
import Scalaz._

object Characterizer {

  sealed trait InvalidUnicode
  private final case class InvalidSurrogate(ch: Char) extends InvalidUnicode
  private final case class UnexpectedCombingChar(cp: Int) extends InvalidUnicode

  private sealed trait CodePointState[+T]
  private final case class HighSurrogateSeen[+T](high: Char, innerState: T) extends CodePointState[T]
  private final case class WaitingForChar[+T](innerState: T) extends CodePointState[T]

  private def toCodePoints[TTerminator, TResult](sequenceHandler: SequenceHandler[InvalidUnicode \/ Int, TTerminator, TResult]): SequenceHandler[Char, TTerminator, TResult] =
    new SequenceHandler[Char, TTerminator, TResult] {
      override type TState = CodePointState[sequenceHandler.TState]

      override def initialState: TState = WaitingForChar(sequenceHandler.initialState)

      override def next(item: Char, state: TState): TState =
        state match {
          case HighSurrogateSeen(high, innerState) =>
            if(Character.isLowSurrogate(item))
              WaitingForChar(sequenceHandler.next(\/-(Character.toCodePoint(high, item)), innerState))
            else
              WaitingForChar(
                sequenceHandler.next(
                  -\/(InvalidSurrogate(item)),
                  sequenceHandler.next(-\/(InvalidSurrogate(high)), innerState)
                )
              )

          case WaitingForChar(innerState) =>
            if(Character.isHighSurrogate(item))
              HighSurrogateSeen(item, innerState)
            else if(Character.isLowSurrogate(item))
              WaitingForChar(sequenceHandler.next(-\/(InvalidSurrogate(item)), innerState))
            else
              WaitingForChar(sequenceHandler.next(\/-(item.toInt), innerState))
        }

      override def end(terminator: TTerminator, state: TState): TResult =
        state match {
          case HighSurrogateSeen(high, innerState) => sequenceHandler.end(terminator, sequenceHandler.next(-\/(InvalidSurrogate(high)), innerState))
          case WaitingForChar(innerState) => sequenceHandler.end(terminator, innerState)
        }
    }

  private sealed trait GraphemeState[+T]
  private final case class WaitingForBaseChar[+T](innerState: T) extends GraphemeState[T]
  private final case class WaitingForCombing[+T](str: String, innerState: T) extends GraphemeState[T]

  private def isBaseCharacterType(cp: Int): Boolean =
    Character.getType(cp) match {
      case Character.NON_SPACING_MARK | Character.COMBINING_SPACING_MARK | Character.ENCLOSING_MARK =>
        false

      case _ =>
        true
    }

  private def getGraphemes[TTerminator, TResult](sequenceHandler: SequenceHandler[InvalidUnicode \/ String, TTerminator, TResult]): SequenceHandler[InvalidUnicode \/ Int, TTerminator, TResult] =
    new SequenceHandler[InvalidUnicode \/ Int, TTerminator, TResult] {
      override type TState = GraphemeState[sequenceHandler.TState]

      override def initialState: TState = WaitingForBaseChar(sequenceHandler.initialState)

      override def next(item: InvalidUnicode \/ Int, state: TState): TState =
        (item, state) match {
          case (-\/(error), WaitingForBaseChar(innerState)) =>
            WaitingForBaseChar(sequenceHandler.next(-\/(error), innerState))
          case (-\/(error), WaitingForCombing(_, innerState)) =>
            WaitingForBaseChar(sequenceHandler.next(-\/(error), innerState))

          case (\/-(cp), WaitingForBaseChar(innerState)) =>
            if(isBaseCharacterType(cp))
              WaitingForCombing(new String(Character.toChars(cp)), innerState)
            else
              WaitingForBaseChar(sequenceHandler.next(-\/(UnexpectedCombingChar(cp)), innerState))

          case (\/-(cp), WaitingForCombing(str, innerState)) =>
            if(isBaseCharacterType(cp))
              WaitingForCombing(new String(Character.toChars(cp)), sequenceHandler.next(\/-(str), innerState))
            else
              WaitingForCombing(str + new String(Character.toChars(cp)), innerState)
        }

      override def end(terminator: TTerminator, state: TState): TResult =
        state match {
          case WaitingForBaseChar(innerState) => sequenceHandler.end(terminator, innerState)
          case WaitingForCombing(str, innerState) => sequenceHandler.end(terminator, sequenceHandler.next(\/-(str), innerState))
        }
    }

  private def nextPosition(ch: String, pos: FilePosition): FilePosition =
    if(ch === "\n")
      FilePosition(pos.line + 1, 1)
    else
      FilePosition(pos.line, pos.position + 1)

  private def posToLoc(pos: FilePosition): SourceLocation =
    SourceLocation(pos, FilePosition(pos.line, pos.position + 1))

  private def withLocation[TResult](sequenceHandler: SequenceHandler[WithSource[String], FilePosition, TResult]): SequenceHandler[InvalidUnicode \/ String, Any, SyntaxError \/ TResult] =
    new SequenceHandler[InvalidUnicode \/ String, Any, SyntaxError \/ TResult] {
      override type TState = SyntaxError \/ (FilePosition, sequenceHandler.TState)

      override def initialState: TState = \/-((FilePosition(1, 1), sequenceHandler.initialState))

      override def next(item: InvalidUnicode \/ String, state: TState): TState =
        state.flatMap { case (pos, innerState) =>
          val loc = posToLoc(pos)

          item
            .leftMap {
              case InvalidSurrogate(ch) => SyntaxError.InvalidSurrogatePairs(ch, loc)
              case UnexpectedCombingChar(cp) => SyntaxError.UnexpectedCombingCharacter(cp, loc)
            }
            .map { str =>
              (nextPosition(str, pos), sequenceHandler.next(WithSource(str, loc), innerState))
            }
        }

      override def end(terminator: Any, state: TState): SyntaxError \/ TResult =
        state.map((sequenceHandler.end _).tupled)
    }

  def characterize[TResult](sequenceHandler: SequenceHandler[WithSource[String], FilePosition, SyntaxError \/ TResult]): SequenceHandler[Char, Any, SyntaxError \/ TResult] =
    toCodePoints(getGraphemes(withLocation(sequenceHandler))).map { _.flatMap(identity) }

}
