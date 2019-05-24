package dev.argon.parser.impl

import cats._
import cats.data._
import cats.implicits._
import dev.argon.util.stream._
import dev.argon.util.{FilePosition, SourceLocation, WithSource}


object Characterizer {

  private def toCodePoints: StreamTransformation[PureEffect, Any, Nothing, Char, Unit, Int, Unit] =
    new StreamTransformation.PureSingle[Nothing, Char, Unit, Int, Unit] {
      override type State = Option[Char]


      override def initialPure: Option[Char] = None


      override def stepSinglePure(s: Option[Char], a: Char): StepPure[Option[Char], Nothing, Nothing, Int, Unit] =
        s match {
          case Some(prevCh) => Step.Produce(None, Character.toCodePoint(prevCh, a), Vector.empty)

          case None =>
            if(Character.isHighSurrogate(a))
              Step.Continue(Some(a))
            else
              Step.Produce(None, a.toInt, Vector.empty)
        }


      override def endPure(s: Option[Char], result: Unit): (Vector[Int], Either[Nothing, Unit]) =
        (s.map { _.toInt }.toList.toVector, Right(()))

    }

  private def toGraphemes: StreamTransformation[PureEffect, Any, Nothing, Int, Unit, String, Unit] =
    new StreamTransformation.PureSingle[Nothing, Int, Unit, String, Unit] {
      override type State = Option[String]


      override def initialPure: Option[String] = None

      override def stepSinglePure(s: Option[String], a: Int): StepPure[Option[String], Nothing, Int, String, Unit] =
        s match {
          case Some(str) if isCombiningChar(a) => Step.Continue(Some(str + codePointToString(a)))
          case Some(str) => Step.Produce(None, str, Vector(a))
          case None => Step.Continue(Some(codePointToString(a)))
        }

      override def endPure(s: Option[String], result: Unit): (Vector[String], Either[Nothing, Unit]) =
        (s.toList.toVector, Right(()))

    }


  private def withSource: StreamTransformation[PureEffect, Any, Nothing, String, Unit, WithSource[String], FilePosition] =
    new StreamTransformation.PureSingle[Nothing, String, Unit, WithSource[String], FilePosition] {
      override type State = FilePosition

      override def initialPure: FilePosition = FilePosition(1, 1)

      override def stepSinglePure(pos: FilePosition, item: String): StepPure[FilePosition, Nothing, String, WithSource[String], FilePosition] = {
        val nextPos =
          if(item === "\n")
            FilePosition(pos.line + 1, 1)
          else
            pos.copy(position = pos.position + 1)

        val newItem = WithSource(item, SourceLocation(pos, nextPos))

        Step.Produce(nextPos, newItem, Vector.empty)
      }

      override def endPure(s: FilePosition, result: Unit): (Vector[WithSource[String]], Either[Nothing, FilePosition]) =
        (Vector.empty, Right(s))

    }


  private def isCombiningChar(cp: Int): Boolean =
    Character.getType(cp) match {
      case Character.COMBINING_SPACING_MARK | Character.ENCLOSING_MARK | Character.NON_SPACING_MARK => true
      case _ => false
    }

  private def codePointToString(cp: Int): String =
    new String(Character.toChars(cp))


  def characterize: StreamTransformation[PureEffect, Any, Nothing, Char, Unit, WithSource[String], FilePosition] =
    toCodePoints.into(toGraphemes).into(withSource)

}
