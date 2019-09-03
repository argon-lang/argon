package dev.argon.parser.impl

import cats._
import cats.data._
import cats.implicits._
import dev.argon.stream.builder._
import dev.argon.stream.{Step, StepPure, StreamTransformation}
import dev.argon.util._


object Characterizer {

  private def toCodePoints[F[_], L[_, _]](chars: L[Char, Unit])(implicit iter: Iter[F, L, Unit]): Generator[F, Int, Unit] = new Generator[F, Int, Unit] {
    override def create[G[_]](implicit genEffect: GenEffect[F, G], builder: Builder[G, Int]): G[Unit] =
      genEffect.liftFuncState(iter.foldLeftMHandlerFunc(chars))(Option.empty[Char]) {
        case (Some(prevCh), ch) => builder.append(Character.toCodePoint(prevCh, ch)).map { _ => None }
        case (None, ch) if Character.isHighSurrogate(ch) => builder.pure(Some(ch))
        case (None, ch) => builder.append(ch.toInt).map { _ => None }
      }.flatMap {
        case (Some(ch), _) => builder.append(ch.toInt)
        case (None, _) => builder.pure(())
      }
  }

  private def toGraphemes[F[_], L[_, _]](codepoints: L[Int, Unit])(implicit iter: Iter[F, L, Unit]): Generator[F, String, Unit] = new Generator[F, String, Unit] {
    override def create[G[_]](implicit genEffect: GenEffect[F, G], builder: Builder[G, String]): G[Unit] =
      genEffect.liftFuncState(iter.foldLeftMHandlerFunc(codepoints))(Option.empty[String]) {
        case (Some(str), cp) if isCombiningChar(cp) => builder.pure(Some(str + codePointToString(cp)))
        case (Some(str), cp) => builder.append(str).map { _ => Some(codePointToString(cp)) }
        case (None, cp) => builder.pure(Some(codePointToString(cp)))
      }.flatMap {
        case (Some(str), _) => builder.append(str)
        case (None, _) => builder.pure(())
      }
  }

  private def withSource[F[_], L[_, _]](graphemes: L[String, Unit])(implicit iter: Iter[F, L, Unit]): Generator[F, WithSource[String], FilePosition] = new Generator[F, WithSource[String], FilePosition] {
    override def create[G[_]](implicit genEffect: GenEffect[F, G], builder: Builder[G, WithSource[String]]): G[FilePosition] =
      genEffect.liftFuncState(iter.foldLeftMHandlerFunc(graphemes))(FilePosition(1, 1)) { (pos, item) =>
        val nextPos =
          if(item === "\n")
            FilePosition(pos.line + 1, 1)
          else
            pos.copy(position = pos.position + 1)

        val newItem = WithSource(item, SourceLocation(pos, nextPos))

        builder.append(newItem).map { _ => nextPos }
      }.map { case (pos, _) => pos }
  }

  private def isCombiningChar(cp: Int): Boolean =
    Character.getType(cp) match {
      case Character.COMBINING_SPACING_MARK | Character.ENCLOSING_MARK | Character.NON_SPACING_MARK => true
      case _ => false
    }

  private def codePointToString(cp: Int): String =
    new String(Character.toChars(cp))


  def characterize[F[_]: Monad, L[_, _]](chars: L[Char, Unit])(implicit iter: Iter[F, L, Unit]): Generator[F, WithSource[String], FilePosition] =
    toCodePoints(chars).into(toGraphemes(_)).into(withSource(_))

}
