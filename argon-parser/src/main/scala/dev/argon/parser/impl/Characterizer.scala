package dev.argon.parser.impl

import scalaz._
import Scalaz._
import dev.argon.util.stream.{ArStream, StreamTransformation}
import dev.argon.util.{FilePosition, NonEmptyVector, SourceLocation, WithSource}


object Characterizer {

  private def toCodePoints[F[_]]: StreamTransformation[F, Char, Unit, Int, Unit] =
    new StreamTransformation.Single[F, Char, Unit, Int, Unit] {
      override type S = Option[Char]

      override val initialState: Option[Char] = None


      override protected def processItem[S2](state: Option[Char], state2: S2, item: Char)(f: (S2, NonEmptyVector[Int]) => F[S2])(implicit monadInstance: Monad[F]): F[(Option[Char], S2)] =
        state match {
          case Some(prevCh) =>
            f(state2, NonEmptyVector.of(Character.toCodePoint(prevCh, item))).map { state2 => (None, state2) }

          case None =>
            if(Character.isHighSurrogate(item))
              (item.point[Option], state2).point[F]
            else
              f(state2, NonEmptyVector.of(item.toInt)).map { state2 => (Option.empty[Char], state2) }
        }

      override def processResult[S2](state: Option[Char], state2: S2, result: Unit)(f: (S2, NonEmptyVector[Int]) => F[S2])(implicit monadInstance: Monad[F]): F[(Unit, S2)] =
        state
          .traverse { ch => f(state2, NonEmptyVector.of(ch.toInt)) }
          .map { newState2 => ((), newState2.getOrElse(state2)) }

    }

  private def toGraphemes[F[_]]: StreamTransformation[F, Int, Unit, String, Unit] =
    new StreamTransformation.Single[F, Int, Unit, String, Unit] {
      override type S = Option[String]

      override val initialState: Option[String] = None


      override protected def processItem[S2](state: Option[String], state2: S2, item: Int)(f: (S2, NonEmptyVector[String]) => F[S2])(implicit monadInstance: Monad[F]): F[(Option[String], S2)] =
        state match {
          case Some(str) if isCombiningChar(item) => ((str + codePointToString(item)).point[Option], state2).point[F]
          case _ => f(state2, NonEmptyVector.of(codePointToString(item))).map { state2 => (Option.empty[String], state2) }
        }

      override def processResult[S2](state: Option[String], state2: S2, result: Unit)(f: (S2, NonEmptyVector[String]) => F[S2])(implicit monadInstance: Monad[F]): F[(Unit, S2)] =
        state
          .traverse { s => f(state2, NonEmptyVector.of(s)) }
          .map { newState2 => ((), newState2.getOrElse(state2)) }

    }


  private def withSource[F[_]]: StreamTransformation[F, String, Unit, WithSource[String], FilePosition] =
    new StreamTransformation.Single[F, String, Unit, WithSource[String], FilePosition] {
      override type S = FilePosition

      override val initialState: FilePosition = FilePosition(1, 1)

      override protected def processItem[S2](pos: FilePosition, state2: S2, item: String)(f: (S2, NonEmptyVector[WithSource[String]]) => F[S2])(implicit monadInstance: Monad[F]): F[(FilePosition, S2)] = {
        val nextPos =
          if(item === "\n")
            FilePosition(pos.line + 1, 1)
          else
            pos.copy(position = pos.position + 1)

        val newItem = WithSource(item, SourceLocation(pos, nextPos))

        f(state2, NonEmptyVector.of(newItem)).map { state2 => (nextPos, state2) }
      }

      override def processResult[S2](pos: FilePosition, state2: S2, result: Unit)(f: (S2, NonEmptyVector[WithSource[String]]) => F[S2])(implicit monadInstance: Monad[F]): F[(FilePosition, S2)] =
        (pos, state2).point[F]
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
