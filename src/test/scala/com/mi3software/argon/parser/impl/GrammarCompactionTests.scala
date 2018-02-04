package com.mi3software.argon.parser.impl

import com.mi3software.argon.parser.GrammarError
import com.mi3software.argon.parser.impl.Grammar.Operators._
import com.mi3software.argon.util.{FilePosition, SourceLocation, WithSource}
import org.scalatest.{FlatSpec, Matchers}

//import scala.language.postfixOps
import scalaz._

class GrammarCompactionTests extends FlatSpec with Matchers {

  type TGrammar = Grammar[Int, GrammarError[Int, String], Int]

  private implicit val errorFactory = new Grammar.ErrorFactory[Int, String, GrammarError[Int, String]] {
    override def createError(error: GrammarError[Int, String]): GrammarError[Int, String] = error

    override def errorEndLocationOrder: Order[GrammarError[Int, String]] =
      (a, b) => implicitly[Order[FilePosition]].order(a.location.end, b.location.end)
  }

  private def numberToken(n: Int): TGrammar = Grammar.matcher(n.toString, m => Some(m).filter(_ === n))

  private val pos = FilePosition(0, 0)

  private def ws[T](t: T) = WithSource(t, SourceLocation.empty)

  "A concat grammar" should "compact to reject for failed prefix" in {
    (numberToken(5) ++ numberToken(6)).derive(ws(4)).compact(pos).toString should startWith ("Reject")
  }

}
