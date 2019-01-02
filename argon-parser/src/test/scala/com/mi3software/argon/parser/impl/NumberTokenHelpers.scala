package com.mi3software.argon.parser.impl

import com.mi3software.argon.util.{FilePosition, SourceLocation, WithSource}
import scalaz._
import Scalaz._
import com.mi3software.argon.grammar.{Grammar, GrammarError}

trait NumberTokenHelpers extends GrammarTestHelpers {

  override type TToken = Int
  override type TSyntaxError = WithSource[String]
  override type TLabel = NumberTokenHelpers.NumberTokenLabel

  override protected val grammarFactory: Grammar.GrammarFactory[Int, WithSource[String], NumberTokenHelpers.NumberTokenLabel] =
    new Grammar.GrammarFactory[Int, WithSource[String], NumberTokenHelpers.NumberTokenLabel] {

      override protected def createGrammar[T](label: NumberTokenHelpers.NumberTokenLabel { type RuleType = T }): TGrammar[T] =
        throw new Exception("No labels exist")

    }


  protected implicit val errorFactory: Grammar.ErrorFactory[Int, String, WithSource[String]] = new Grammar.ErrorFactory[Int, String, WithSource[String]] {
    override def createError(error: GrammarError[Int, String]): WithSource[String] =
      WithSource(s"$error", error.location)

    override def createAmbiguityError(location: SourceLocation): WithSource[String] =
      WithSource("Ambiguity", location)

    override def errorEndLocationOrder: Order[WithSource[String]] =
      (a, b) => implicitly[Order[FilePosition]].order(a.location.end, b.location.end)
  }

  protected def numberToken(n: Int): TGrammar[Int] = Grammar.matcher(n.toString, (m: Int) => Some(m).filter(_ === n))



}

object NumberTokenHelpers {

  sealed trait NumberTokenLabel extends Grammar.RuleLabel {
    override type RuleType = Nothing
  }

}
