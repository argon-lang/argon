package com.mi3software.argon.parser.impl

import com.mi3software.argon.parser.GrammarError
import com.mi3software.argon.parser.impl.Grammar.Operators._
import com.mi3software.argon.util.{FilePosition, SourceLocation, WithSource}
import org.scalatest.{FlatSpec, Matchers}

//import scala.language.postfixOps
import scalaz._

class GrammarCompactionTests extends FlatSpec with Matchers {

  type TGrammar = Grammar[Int, WithSource[String], Int]

  private implicit val errorFactory = new Grammar.ErrorFactory[Int, String, WithSource[String]] {
    override def createError(error: GrammarError[Int, String]): WithSource[String] =
      WithSource(error.toString, error.location)

    override def createAmbiguityError(location: SourceLocation): WithSource[String] =
      WithSource("Ambiguity", location)

    override def errorEndLocationOrder: Order[WithSource[String]] =
      (a, b) => implicitly[Order[FilePosition]].order(a.location.end, b.location.end)
  }

  private def numberToken(n: Int): TGrammar = Grammar.matcher(n.toString, m => Some(m).filter(_ === n))

  private val pos = FilePosition(0, 0)

  private def ws[T](t: T) = WithSource(t, SourceLocation.empty)

  "A concat grammar" should "compact to reject for failed prefix" in {
    (numberToken(5) ++ numberToken(6)).derive(ws(4)).compact(pos).toString should startWith ("Reject")
  }

  "A concat grammar" should "compact to remove failed cases" in {
    (numberToken(5) | numberToken(6)).derive(ws(5)).compact(pos).toString should startWith ("EmptyStr")
  }

}
