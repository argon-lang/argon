package com.mi3software.argon.parser.impl

import scalaz._
import Scalaz._
import fs2._

object Characterizer {

  private def toCodePoints[F[_]]: Pipe[F, Char, Int] =
    in => in
      .pull
      .scanSegments(None : Option[Char]) { (prevCharOpt, seg) =>
        seg
          .mapAccumulate(prevCharOpt) {
            case (None, ch) if Character.isHighSurrogate(ch) => (Some(ch), None)
            case (None, ch) => (None, Some(ch.toInt))
            case (Some(prevCh), ch) => (None, Some(Character.toCodePoint(prevCh, ch)))
          }
          .mapResult { case (_, prevCharOpt) => prevCharOpt }
      }
      .flatMap(lastCharOpt => Pull.output1(lastCharOpt.map { _.toInt }))
      .stream
      .unNone

  private def isCombiningChar(cp: Int): Boolean =
    Character.getType(cp) match {
      case Character.COMBINING_SPACING_MARK | Character.ENCLOSING_MARK | Character.NON_SPACING_MARK => true
      case _ => false
    }

  private def codePointToString(cp: Int): String =
    new String(Character.toChars(cp))

  private def toGraphemes[F[_]]: Pipe[F, Int, String] =
    in => in
      .pull
      .scanSegments(None : Option[String]) { (acc, seg) =>
        seg
          .mapAccumulate(acc) {
            case (Some(str), cp) if isCombiningChar(cp) => (Some(str + codePointToString(cp)), None)
            case (strOpt, cp) => (Some(codePointToString(cp)), strOpt)
          }
          .mapResult { case (_, acc) => acc }
      }
      .flatMap(Pull.output1)
      .stream
      .unNone

  private def withLength[F[_]]: Pipe[F, String, WithLength[String]] =
    in => in.map { ch =>
      if(ch === "\n")
        WithLength(ch, 0, lengthAfterNewLine = true)
      else
        WithLength(ch, 1, lengthAfterNewLine = false)
    }

  def characterize[F[_]]: Pipe[F, Char, WithLength[String]] =
    _.through(toCodePoints).through(toGraphemes).through(withLength)

}
