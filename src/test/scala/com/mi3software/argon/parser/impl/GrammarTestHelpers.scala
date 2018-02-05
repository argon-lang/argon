package com.mi3software.argon.parser.impl

import com.mi3software.argon.parser.GrammarError
import com.mi3software.argon.util.{FilePosition, SourceLocation, WithSource}

import scalaz._
import Scalaz._

trait GrammarTestHelpers {

  protected type TGrammar = Grammar[Int, WithSource[String], Int]

  protected implicit val errorFactory: Grammar.ErrorFactory[Int, String, WithSource[String]] = new Grammar.ErrorFactory[Int, String, WithSource[String]] {
    override def createError(error: GrammarError[Int, String]): WithSource[String] =
      WithSource(s"$error", error.location)

    override def createAmbiguityError(location: SourceLocation): WithSource[String] =
      WithSource("Ambiguity", location)

    override def errorEndLocationOrder: Order[WithSource[String]] =
      (a, b) => implicitly[Order[FilePosition]].order(a.location.end, b.location.end)
  }

  protected def numberToken(n: Int): TGrammar = Grammar.matcher(n.toString, m => Some(m).filter(_ === n))

  protected def parse[T](grammar: Grammar[Int, WithSource[String], T])(tokens: Int*): Either[NonEmptyList[WithSource[String]], (Vector[Int], T)] =
    grammar.parse(
      tokens.zipWithIndex.map { case (value, i) => WithSource(value, SourceLocation(FilePosition(1, i + 1), FilePosition(1, i + 2))) }.toVector,
      FilePosition(1, 1)
    )
    .map { case (remTokens, _, res) => (remTokens.map { _.value }, res.value) }

}
