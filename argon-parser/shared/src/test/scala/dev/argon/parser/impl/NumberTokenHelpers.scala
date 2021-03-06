package dev.argon.parser.impl

import dev.argon.util.{FilePosition, WithSource}
import cats._
import cats.implicits._
import dev.argon.grammar.{Grammar, GrammarError}

trait NumberTokenHelpers extends GrammarTestHelpers {

  override type TToken = Int
  override type TSyntaxError = WithSource[String]
  override type TLabel[T] = NumberTokenHelpers.NumberTokenLabel[T]

  override protected val grammarFactory: Grammar.GrammarFactory[Int, WithSource[String], NumberTokenHelpers.NumberTokenLabel] =
    new Grammar.GrammarFactory[Int, WithSource[String], NumberTokenHelpers.NumberTokenLabel] {

      override protected def createGrammar[T](label: NumberTokenHelpers.NumberTokenLabel[T]): TGrammar[T] =
        throw new Exception("No labels exist")

    }


  protected implicit val errorFactory: Grammar.ErrorFactory[Int, String, WithSource[String]] = new Grammar.ErrorFactory[Int, String, WithSource[String]] {
    @SuppressWarnings(Array("scalafix:Disable.toString"))
    override def createError(error: GrammarError[Int, String]): WithSource[String] =
      WithSource(error.toString, error.location)

    override def errorEndLocationOrder: Order[WithSource[String]] =
      (a, b) => implicitly[Order[FilePosition]].compare(a.location.end, b.location.end)
  }

  protected def numberToken(n: Int): TGrammar[Int] = Grammar.matcher(n.toString, (m: Int) => Some(m).filter(_ === n))



}

object NumberTokenHelpers {

  sealed trait NumberTokenLabel[T]

}
