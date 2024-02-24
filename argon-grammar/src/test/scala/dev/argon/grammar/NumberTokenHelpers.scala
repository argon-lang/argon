package dev.argon.grammar

import dev.argon.grammar.{Grammar, GrammarError}
import dev.argon.util.{FilePosition, WithLocation, WithSource}

trait NumberTokenHelpers extends GrammarTestHelpers {

  override type TToken = Int
  override type TLabel[T] = NumberTokenHelpers.NumberTokenLabel[T]

  protected override val grammarFactory
    : Grammar.GrammarFactory[Int, String, FilePosition, NumberTokenHelpers.NumberTokenLabel] =
    new Grammar.GrammarFactory[Int, String, FilePosition, NumberTokenHelpers.NumberTokenLabel] {

      override val fileName: Option[String] = None

      protected override def createGrammar[T](label: NumberTokenHelpers.NumberTokenLabel[T]): TGrammar[T] =
        throw new Exception("No labels exist")

    }

  protected def numberToken(n: Int): TGrammar[Int] = Grammar.matcher(n.toString, (m: Int) => Some(m).filter(_ == n))

}

object NumberTokenHelpers {

  sealed trait NumberTokenLabel[T]

}
