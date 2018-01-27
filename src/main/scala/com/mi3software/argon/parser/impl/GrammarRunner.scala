package com.mi3software.argon.parser.impl

import com.mi3software.argon.parser.{CharacterCategory, SyntaxError}
import com.mi3software.argon.util.{FilePosition, SourceLocation, WithSource}

import scalaz._
import Scalaz._

object GrammarRunner {

  type TGrammar[T] = Grammar[String, CharacterCategory, T]

  private sealed trait InvalidUnicode
  private final case class InvalidSurrogate(ch: Char) extends InvalidUnicode
  private final case class UnexpectedCombingChar(cp: Int) extends InvalidUnicode

  private def toCodePoints(chars: Stream[Char]): Stream[InvalidUnicode \/ Int] =
    chars match {
      case high #:: low #:: tail if Character.isHighSurrogate(high) && Character.isLowSurrogate(low) =>
        \/-(Character.toCodePoint(high, low)) #:: toCodePoints(tail)

      case ch #:: _ if Character.isSurrogate(ch) =>
        Stream(-\/(InvalidSurrogate(ch)))

      case ch #:: tail =>
        \/-(ch.toInt) #:: toCodePoints(tail)
    }

  private def isBaseCharacterType(cp: Int): Boolean =
    Character.getType(cp) match {
      case Character.NON_SPACING_MARK | Character.COMBINING_SPACING_MARK | Character.ENCLOSING_MARK =>
        false

      case _ =>
        true
    }

  private def appendCombingChars(s: String, cps: Stream[InvalidUnicode \/ Int]): Stream[InvalidUnicode \/ String] =
    cps match {
      case -\/(err) #:: _ => Stream(-\/(err))
      case \/-(cp) #:: tail if !isBaseCharacterType(cp) =>
        appendCombingChars(s + new String(Character.toChars(cp)), tail)
      case _ =>
        \/-(s) #:: getGraphemes(cps)
    }

  private def getGraphemes(cps: Stream[InvalidUnicode \/ Int]): Stream[InvalidUnicode \/ String] =
    cps match {
      case -\/(err) #:: _ => Stream(-\/(err))
      case \/-(cp) #:: tail if isBaseCharacterType(cp) =>
        appendCombingChars(new String(Character.toChars(cp)), tail)

      case \/-(cp) #:: tail =>
        Stream(-\/(UnexpectedCombingChar(cp)))

      case _ => Stream()
    }

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


  private def processLocation[T](pos: FilePosition, grammar: TGrammar[T], chars: Stream[InvalidUnicode \/ String]): NonEmptyList[SyntaxError] \/ T =
    if(grammar.shortCircuit)
      grammarResult(pos, grammar)
    else {
      val chEnd = FilePosition(pos.line, pos.position + 1)
      val location = SourceLocation(pos, chEnd)
      chars match {
        case -\/(InvalidSurrogate(ch)) #:: _ =>
          -\/(NonEmptyList(SyntaxError.InvalidSurrogatePairs(ch, location)))

        case -\/(UnexpectedCombingChar(cp)) #:: _ =>
          -\/(NonEmptyList(SyntaxError.UnexpectedCombingCharacter(cp, location)))

        case \/-(ch) #:: tail =>
          processLocation(nextPosition(ch, pos), grammar.derive(WithSource(ch, location)), tail)

        case _ =>
          grammarResult(pos, grammar)
      }
    }


  def runGrammar[T](grammar: TGrammar[T])(chars: Stream[Char]): NonEmptyList[SyntaxError] \/ T =
    processLocation(FilePosition(1, 1), grammar, getGraphemes(toCodePoints(chars)))

}
