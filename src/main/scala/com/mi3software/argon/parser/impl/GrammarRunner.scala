package com.mi3software.argon.parser.impl

import com.mi3software.argon.parser.{CharacterCategory, SyntaxError}
import com.mi3software.argon.util.{FilePosition, SourceLocation, WithSource}

import scalaz._
import Scalaz._

import com.thoughtworks.each.Monadic._

object GrammarRunner {

  type TGrammar[T] = Grammar[String, CharacterCategory, T]

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

  private def grammarResult[T](pos: FilePosition, grammar: TGrammar[T]): NonEmptyList[SyntaxError] \/ T =
    grammar
      .endOfInput(pos)
      .leftMap(_.map(error => SyntaxError.LexerError(error) : SyntaxError))
      .map(results => results.head.value)


  private def processLocation[M[_] : Monad, T](pos: FilePosition, grammar: TGrammar[T], chars: StreamT[M, InvalidUnicode \/ String]): M[NonEmptyList[SyntaxError] \/ T] = monadic[M] {
    if(grammar.shortCircuit)
      grammarResult(pos, grammar)
    else {
      val chEnd = FilePosition(pos.line, pos.position + 1)
      val location = SourceLocation(pos, chEnd)
      chars.uncons.each match {
        case Some((-\/(InvalidSurrogate(ch)), _)) =>
          -\/(NonEmptyList(SyntaxError.InvalidSurrogatePairs(ch, location)))

        case Some((-\/(UnexpectedCombingChar(cp)), _)) =>
          -\/(NonEmptyList(SyntaxError.UnexpectedCombingCharacter(cp, location)))

        case Some((\/-(ch), tail)) =>
          processLocation(nextPosition(ch, pos), grammar.derive(WithSource(ch, location)), tail).each

        case _ =>
          grammarResult(pos, grammar)
      }
    }
  }


  def runGrammar[M[_] : Monad, T](grammar: TGrammar[T])(chars: StreamT[M, Char]): M[NonEmptyList[SyntaxError] \/ T] =
    processLocation(FilePosition(1, 1), grammar, getGraphemes(toCodePoints(chars)))

}
