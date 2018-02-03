package com.mi3software.argon.parser

import com.mi3software.argon.parser.impl.{Characterizer, Grammar, Lexer, Parser}
import com.mi3software.argon.util.{EitherTFlattener, EitherTLeftMapper, WithSource}

import scalaz._

final class ParseHandler {
  /*
  val lexer = new Lexer()
  val parser = new Parser()

  private def singleToListMapper[M[_] : Monad, T] =
    new EitherTLeftMapper[M, T, NonEmptyList[T]](item => NonEmptyList(item))

  private def lexerErrorMapper[M[_] : Monad]: (EitherT[M, NonEmptyList[GrammarError[String, CharacterCategory]], ?] ~> EitherT[M, NonEmptyList[SyntaxError], ?]) =
    new EitherTLeftMapper[M, NonEmptyList[GrammarError[String, CharacterCategory]], NonEmptyList[SyntaxError]](items => items.map(SyntaxError.LexerError.apply))

  def parse[M[_] : Monad](chars: StreamT[M, Char]): StreamT[EitherT[M, NonEmptyList[SyntaxError], ?], WithSource[IList[WithSource[Token]]]] = {

    implicit val eitherEitherMonadInstance1 = EitherT.eitherTMonad[EitherT[M, NonEmptyList[SyntaxError], ?], NonEmptyList[GrammarError[String, CharacterCategory]]]
    implicit val eitherEitherMonadInstance2 = EitherT.eitherTMonad[EitherT[M, NonEmptyList[SyntaxError], ?], NonEmptyList[SyntaxError]]

    val chs = Characterizer.characterize(chars)
      .trans[EitherT[M, NonEmptyList[SyntaxError], ?]](singleToListMapper)

    lexer.lexer.stream[EitherT[M, NonEmptyList[SyntaxError], ?]](chs)
      .trans[EitherT[EitherT[M, NonEmptyList[SyntaxError], ?], NonEmptyList[SyntaxError], ?]](lexerErrorMapper[EitherT[M, NonEmptyList[SyntaxError], ?]])
      .trans[EitherT[M, NonEmptyList[SyntaxError], ?]](new EitherTFlattener[M, NonEmptyList[SyntaxError]])
  }
*/



}
