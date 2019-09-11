package dev.argon.parser.impl

import cats._
import cats.data._
import cats.implicits._
import dev.argon.stream.builder._
import dev.argon.stream.{Step, StepPure, StreamTransformation}
import dev.argon.util._


object Characterizer {

  private def toCodePoints[F[_]: Monad](chars: Source[F, Char, Unit]): Source[F, Int, Unit] = new Source[F, Int, Unit] {

    override protected val monadF: Monad[F] = implicitly

    override protected def generateImpl[G[_] : Monad](sink: Sink[G, Int])(implicit genEffect: GenEffect[F, G]): G[Unit] =
      chars.foldLeftG(Option.empty[Char]) {
        case (Some(prevCh), ch) => sink.consume(Character.toCodePoint(prevCh, ch)).map { _ => Option.empty[Char] }
        case (None, ch) if Character.isHighSurrogate(ch) => (Some(ch) : Option[Char]).pure[G]
        case (None, ch) => sink.consume(ch.toInt).map { _ => Option.empty[Char] }
      }.flatMap {
        case (Some(ch), _) => sink.consume(ch.toInt)
        case (None, _) => Monad[G].pure(())
      }
  }

  private def toGraphemes[F[_]: Monad](codepoints: Source[F, Int, Unit]): Source[F, String, Unit] = new Source[F, String, Unit] {

    override protected val monadF: Monad[F] = implicitly

    override protected def generateImpl[G[_] : Monad](sink: Sink[G, String])(implicit genEffect: GenEffect[F, G]): G[Unit] =
      codepoints.foldLeftG[G, Option[String]](Option.empty[String]) {
        case (Some(str), cp) if isCombiningChar(cp) => Monad[G].pure(Some(str + codePointToString(cp)))
        case (Some(str), cp) => sink.consume(str).map { _ => Some(codePointToString(cp)) }
        case (None, cp) => Monad[G].pure(Some(codePointToString(cp)))
      }.flatMap {
        case (Some(str), _) => sink.consume(str)
        case (None, _) => Monad[G].pure(())
      }
  }

  private def withSource[F[_]: Monad](graphemes: Source[F, String, Unit]): Source[F, WithSource[String], FilePosition] = new Source[F, WithSource[String], FilePosition] {

    override protected val monadF: Monad[F] = implicitly

    override protected def generateImpl[G[_] : Monad](sink: Sink[G, WithSource[String]])(implicit genEffect: GenEffect[F, G]): G[FilePosition] =
      graphemes.foldLeftG(FilePosition(1, 1)) { (pos, item) =>
        val nextPos =
          if(item === "\n")
            FilePosition(pos.line + 1, 1)
          else
            pos.copy(position = pos.position + 1)

        val newItem = WithSource(item, SourceLocation(pos, nextPos))

        sink.consume(newItem).map { _ => nextPos }
      }.map { case (pos, _) => pos }
  }

  private def isCombiningChar(cp: Int): Boolean =
    Character.getType(cp) match {
      case Character.COMBINING_SPACING_MARK | Character.ENCLOSING_MARK | Character.NON_SPACING_MARK => true
      case _ => false
    }

  private def codePointToString(cp: Int): String =
    new String(Character.toChars(cp))


  def characterize[F[_]: Monad](chars: Source[F, Char, Unit]): Source[F, WithSource[String], FilePosition] =
    chars.into(toCodePoints(_)).into(toGraphemes(_)).into(withSource(_))

}
