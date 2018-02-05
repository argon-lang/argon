package com.mi3software.argon.parser.impl

import org.scalatest.{FlatSpec, Matchers}

import scalaz._
import Grammar.Operators._

import scala.language.postfixOps

class GrammarTests extends FlatSpec with Matchers with GrammarTestHelpers {

  "A token grammar" should "fail for EOF" in {
    parse(numberToken(5))() should matchPattern { case Left(_) => }
  }

  it should "fail for wrong token" in {
    parse(numberToken(5))(8) should matchPattern { case Left(_) => }
  }

  it should "succeed for correct token" in {
    parse(numberToken(5))(5) shouldBe Right((Vector(), 5))
  }

  "An optional token grammar" should "succeed for EOF" in {
    parse(numberToken(6)?)() shouldBe Right((Vector(), None))
  }

  it should "not consume wrong token" in {
    parse(numberToken(6)?)(2) shouldBe Right((Vector(2), None))
  }

  it should "succeed for correct token" in {
    parse(numberToken(6)?)(6) shouldBe Right((Vector(), Some(6)))
  }

  "A repeated (*) token grammar" should "succeed for EOF" in {
    parse(numberToken(7)*)() shouldBe Right((Vector(), Vector()))
  }

  it should "not consume wrong token" in {
    parse(numberToken(7)*)(4) shouldBe Right((Vector(4), Vector()))
  }

  it should "succeed for correct token" in {
    parse(numberToken(7)*)(7) shouldBe Right((Vector(), Vector(7)))
  }

  it should "succeed for 2 correct tokens" in {
    parse(numberToken(7)*)(7, 7) shouldBe Right((Vector(), Vector(7, 7)))
  }

  "A repeated (+~) token grammar" should "fail for EOF" in {
    parse(numberToken(7)+~)() should matchPattern { case Left(_) => }
  }

  it should "fail for wrong token" in {
    parse(numberToken(7)+~)(4) should matchPattern { case Left(_) => }
  }

  it should "succeed for correct token" in {
    parse(numberToken(7)+~)(7) shouldBe Right((Vector(), NonEmptyList(7)))
  }

  it should "succeed for 2 correct tokens" in {
    parse(numberToken(7)+~)(7, 7) shouldBe Right((Vector(), NonEmptyList(7, 7)))
  }

  "A union grammar" should "fail for EOF" in {
    parse(numberToken(0) | numberToken(1))() should matchPattern { case Left(_) => }
  }

  it should "fail for wrong token" in {
    parse(numberToken(0) | numberToken(1))(2) should matchPattern { case Left(_) => }
  }

  it should "succeed for left token" in {
    parse(numberToken(0) | numberToken(1))(0) shouldBe Right((Vector(), 0))
  }

  it should "succeed for right token" in {
    parse(numberToken(0) | numberToken(1))(1) shouldBe Right((Vector(), 1))
  }

  "A concat grammar" should "fail for EOF" in {
    parse(numberToken(4) ++ numberToken(8))() should matchPattern { case Left(_) => }
  }

  it should "fail for wrong token" in {
    parse(numberToken(4) ++ numberToken(8))(2) should matchPattern { case Left(_) => }
  }

  it should "fail for wrong first token" in {
    parse(numberToken(4) ++ numberToken(8))(2, 8) should matchPattern { case Left(_) => }
  }

  it should "fail for wrong second token" in {
    parse(numberToken(4) ++ numberToken(8))(4, 7) should matchPattern { case Left(_) => }
  }

  it should "fail for first token" in {
    parse(numberToken(4) ++ numberToken(8))(4) should matchPattern { case Left(_) => }
  }

  it should "fail for second token" in {
    parse(numberToken(4) ++ numberToken(8))(8) should matchPattern { case Left(_) => }
  }

  it should "succeed for first and second token" in {
    parse(numberToken(4) ++ numberToken(8))(4, 8) shouldBe Right((Vector(), (4, 8)))
  }

  private lazy val leftRec: TGrammar =
    leftRec -- numberToken(1) --> { case (a, b) => a + b } | numberToken(0)

  "An left recursive grammar" should "fail for EOF" in {
    parse(leftRec)() should matchPattern { case Left(_) => }
  }

  it should "fail for wrong token" in {
    parse(leftRec)(2) should matchPattern { case Left(_) => }
  }

  it should "fail for tail token" in {
    parse(leftRec)(1) should matchPattern { case Left(_) => }
  }

  it should "succeed for head token" in {
    parse(leftRec)(0) shouldBe Right((Vector(), 0))
  }

  it should "succeed for head tail" in {
    parse(leftRec)(0, 1) shouldBe Right((Vector(), 1))
  }

  it should "not consume second head for head head" in {
    parse(leftRec)(0, 0) shouldBe Right((Vector(0), 0))
  }

  it should "succeed for head tail tail" in {
    parse(leftRec)(0, 1, 1) shouldBe Right((Vector(), 2))
  }

  private lazy val rightRec: TGrammar =
    numberToken(0) | numberToken(1) ++ rightRec --> { case (a, b) => a + b }

  "A right recursive grammar" should "fail for EOF" in {
    parse(rightRec)() should matchPattern { case Left(_) => }
  }

  it should "fail for wrong token" in {
    parse(rightRec)(2) should matchPattern { case Left(_) => }
  }

  it should "fail for prefix token" in {
    parse(rightRec)(1) should matchPattern { case Left(_) => }
  }

  it should "succeed for end token" in {
    parse(rightRec)(0) shouldBe Right((Vector(), 0))
  }

  it should "succeed for prefix end" in {
    parse(rightRec)(1, 0) shouldBe Right((Vector(), 1))
  }

  it should "not consume second end for end end" in {
    parse(rightRec)(0, 0) shouldBe Right((Vector(0), 0))
  }

  it should "succeed for prefix prefix end" in {
    parse(rightRec)(1, 1, 0) shouldBe Right((Vector(), 2))
  }

}
