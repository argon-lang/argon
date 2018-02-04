package com.mi3software.argon.parser.impl

import com.mi3software.argon.parser.impl.Grammar.Operators._
import org.scalatest.{FlatSpec, Matchers}

class GrammarCompactionTests extends FlatSpec with Matchers with GrammarTestHelpers {

  "A concat grammar" should "compact to reject for failed prefix" in {
    (numberToken(5) ++ numberToken(6)).derive(ws(4)).compact(pos).toString should startWith ("Reject")
  }

  "A concat grammar" should "compact to remove failed cases" in {
    (numberToken(5) | numberToken(6)).derive(ws(5)).compact(pos).toString should startWith ("EmptyStr")
  }

}
