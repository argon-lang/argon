package com.mi3software.argon.parser.impl

import com.mi3software.argon.util._

import scalaz._
import Scalaz._

object Characterizer {

  private def isBaseCharacterType(cp: Int): Boolean =
    Character.getType(cp) match {
      case Character.NON_SPACING_MARK | Character.COMBINING_SPACING_MARK | Character.ENCLOSING_MARK =>
        false

      case _ =>
        true
    }

  private def stringToCodePoints(str: String): Vector[Int] = {

    def iter(i: Int, acc: Vector[Int]): Vector[Int] =
      if(i < str.length) {
        val cp = str.codePointAt(i)
        iter(i + Character.charCount(cp), acc :+ cp)
      }
      else {
        acc
      }


    iter(0, Vector.empty)
  }

  private def getGraphemes(cps: Vector[Int]): Vector[String] = {
    val (graphemes, remaining) = cps.foldLeft((Vector[String](), "")) { case ((graphemes, current), ch) =>
      val chStr = new String(Character.toChars(ch))
      if(isBaseCharacterType(ch) && current.length > 0)
        (graphemes :+ current, chStr)
      else
        (graphemes, current + chStr)
    }
    if (remaining.length > 0)
      graphemes :+ remaining
    else
      graphemes
  }

  private def withSourceLocation(items: Vector[String]): Vector[WithSource[String]] =
    items.foldLeft((Vector[WithSource[String]](), 1, 1)) {
      case ((acc, line, pos), ch) =>
        val item = WithSource(ch, SourceLocation(FilePosition(line, pos), FilePosition(line, pos + 1)))
        if(ch === "\n")
          (acc :+ item, line + 1, 1)
        else
          (acc :+ item, line, pos + 1)
    } match { case (result, _, _) => result }

  def characterize(code: String): Vector[WithSource[String]] =
    withSourceLocation(
      getGraphemes(stringToCodePoints(code))
    )

}
