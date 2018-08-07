package com.mi3software.argon.parser.impl

import com.mi3software.argon.parser.GrammarError
import com.mi3software.argon.util.{FilePosition, SourceLocation, WithSource}
import scalaz._
import Scalaz._
import com.mi3software.argon.parser.impl.Grammar.{GrammarResultComplete, GrammarResultTransform, ParseOptions, ParseState}

trait GrammarTestHelpers {

  protected type TGrammar = Grammar[Int, WithSource[String], Any, Int]

  protected implicit val errorFactory: Grammar.ErrorFactory[Int, String, WithSource[String]] = new Grammar.ErrorFactory[Int, String, WithSource[String]] {
    override def createError(error: GrammarError[Int, String]): WithSource[String] =
      WithSource(s"$error", error.location)

    override def createAmbiguityError(location: SourceLocation): WithSource[String] =
      WithSource("Ambiguity", location)

    override def errorEndLocationOrder: Order[WithSource[String]] =
      (a, b) => implicitly[Order[FilePosition]].order(a.location.end, b.location.end)
  }

  protected def numberToken(n: Int): TGrammar = Grammar.matcher(n.toString, m => Some(m).filter(_ === n))

  def parseTokens[T](grammar: Grammar[Int, WithSource[String], Any, T])(tokens: Vector[WithSource[Int]]): GrammarResultComplete[Int, WithSource[String], Any, T]

  protected def parse[T](grammar: Grammar[Int, WithSource[String], Any, T])(tokens: Int*): Either[NonEmptyList[WithSource[String]], (Vector[Int], T)] =
    parseTokens(grammar)(
      tokens.zipWithIndex.map { case (value, i) => WithSource(value, SourceLocation(FilePosition(1, i + 1), FilePosition(1, i + 2))) }.toVector
    )
      .toEither
      .map { case (ParseState(remTokens, _), res) => (remTokens.map { _.value }, res.value) }


}

trait GrammarTestHelpersEntireSequence extends GrammarTestHelpers {
  override def parseTokens[T](grammar: Grammar[Int, WithSource[String], Any, T])(tokens: Vector[WithSource[Int]]): GrammarResultComplete[Int, WithSource[String], Any, T] =
    if(tokens.isEmpty)
      grammar
        .parseEnd(
          FilePosition(1, 1),
          ParseOptions(Set.empty, None)
        )
    else
      grammar
        .parseTokens(
          ParseState(
            tokens,
            FilePosition(1, 1)
          ),
          ParseOptions(Set.empty, None)
        )
        .completeResult
}

trait GrammarTestHelpersSingleTokens extends GrammarTestHelpers {
  override def parseTokens[T](grammar: Grammar[Int, WithSource[String], Any, T])(tokens: Vector[WithSource[Int]]): GrammarResultComplete[Int, WithSource[String], Any, T] = {
    def impl[A](trans: GrammarResultTransform[Int, WithSource[String], Any, A, T])(tokens: Vector[WithSource[Int]]): GrammarResultComplete[Int, WithSource[String], Any, T] =
      tokens match {
        case head +: tail =>
          trans.grammar.parseTokens(ParseState(Vector(head), trans.pos), trans.parseOptions).transformComplete(trans.f) match {
            case result: GrammarResultComplete[Int, WithSource[String], Any, T] =>
              result.map { case (ParseState(unusedTokens, pos), t) => (ParseState(unusedTokens ++ tail, pos), t) }
            case trans: GrammarResultTransform[Int, WithSource[String], Any, a, T] =>
              impl(trans)(tail)
          }

        case _ =>
          trans.grammar
            .parseEnd(
              trans.pos,
              trans.parseOptions
            )
            .transformComplete(trans.f)
            .completeResult
      }

    impl(GrammarResultTransform(grammar)(ParseOptions(Set.empty, None))(FilePosition(1, 1))(identity))(tokens)
  }
}
