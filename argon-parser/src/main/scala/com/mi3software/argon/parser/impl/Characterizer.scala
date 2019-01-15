package com.mi3software.argon.parser.impl

import scalaz._
import Scalaz._
import com.mi3software.argon.util.stream.{ArStream, StreamTransformation, StreamTransformationM}
import com.mi3software.argon.util.{FilePosition, SourceLocation, WithSource}


object Characterizer {

  private val toCodePoints: StreamTransformation[Char, Unit, Int, Unit] =
    new StreamTransformation.Single[Char, Unit, Int, Unit] {
      override protected type S = Option[Char]

      override protected val initialState: Option[Char] = None

      override protected def processItem(state: Option[Char], item: Char): (Option[Char], Vector[Int]) =
        state match {
          case Some(prevCh) => (None, Vector(Character.toCodePoint(prevCh, item)))
          case None =>
            if(Character.isHighSurrogate(item))
              (Some(item), Vector.empty)
            else
              (None, Vector(item.toInt))
        }

      override protected def processResult(state: Option[Char], result: Unit): (Unit, Vector[Int]) =
        ((), state.map { _.toInt }.toVector)
    }

  private val toGraphemes: StreamTransformation[Int, Unit, String, Unit] =
    new StreamTransformation.Single[Int, Unit, String, Unit] {
      override protected type S = Option[String]

      override protected val initialState: Option[String] = None

      override protected def processItem(state: Option[String], item: Int): (Option[String], Vector[String]) =
        state match {
          case Some(str) if isCombiningChar(item) => (Some(str + codePointToString(item)), Vector.empty)
          case _ => (Some(codePointToString(item)), state.toVector)
        }

      override protected def processResult(state: Option[String], result: Unit): (Unit, Vector[String]) =
        ((), state.toVector)
    }


  private val withSource: StreamTransformation[String, Unit, WithSource[String], FilePosition] =
    new StreamTransformation.Single[String, Unit, WithSource[String], FilePosition] {
      override protected type S = FilePosition

      override protected val initialState: FilePosition = FilePosition(1, 1)

      override protected def processItem(pos: FilePosition, item: String): (FilePosition, Vector[WithSource[String]]) = {
        val nextPos =
          if(item === "\n")
            FilePosition(pos.line + 1, 1)
          else
            pos.copy(position = pos.position + 1)

        (nextPos, Vector(WithSource(item, SourceLocation(pos, nextPos))))
      }

      override protected def processResult(state: FilePosition, result: Unit): (FilePosition, Vector[WithSource[String]]) =
        (state, Vector.empty)
    }


  private def isCombiningChar(cp: Int): Boolean =
    Character.getType(cp) match {
      case Character.COMBINING_SPACING_MARK | Character.ENCLOSING_MARK | Character.NON_SPACING_MARK => true
      case _ => false
    }

  private def codePointToString(cp: Int): String =
    new String(Character.toChars(cp))


  def characterize[F[_]: Monad](stream: ArStream[F, Char, Unit]): ArStream[F, WithSource[String], FilePosition] =
    stream.transformWith(toCodePoints).transformWith(toGraphemes).transformWith(withSource)

}
