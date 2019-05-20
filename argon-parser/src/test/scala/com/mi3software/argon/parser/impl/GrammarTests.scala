package dev.argon.parser.impl

import org.scalatest.{FlatSpec, Matchers}
import scalaz._
import dev.argon.grammar.Grammar
import Grammar.Operators._
import cats.data.NonEmptyVector

import scala.language.postfixOps

abstract class GrammarTestsCommon extends FlatSpec with Matchers with GrammarTestHelpers with NumberTokenHelpers {


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

  it should "succeed with trailing token" in {
    parse(((numberToken(7) ++ numberToken(8))*) ++ numberToken(4))(7, 8, 7, 8, 4) shouldBe Right((Vector(), (Vector((7, 8), (7, 8)), 4)))
  }

  "A repeated (+~) token grammar" should "fail for EOF" in {
    parse(numberToken(7)+~)() should matchPattern { case Left(_) => }
  }

  it should "fail for wrong token" in {
    parse(numberToken(7)+~)(4) should matchPattern { case Left(_) => }
  }

  it should "succeed for correct token" in {
    parse(numberToken(7)+~)(7) shouldBe Right((Vector(), NonEmptyVector.of(7)))
  }

  it should "succeed for 2 correct tokens" in {
    parse(numberToken(7)+~)(7, 7) shouldBe Right((Vector(), NonEmptyVector.of(7, 7)))
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

  private lazy val rightRec: TGrammar[Int] =
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

  "A strict grammar" should "succeed when first option fails" in {
    parse(numberToken(3) ++! (numberToken(9) | numberToken(1)))(3, 1) shouldBe Right((Vector(), (3, 1)))
  }

  it should "succeed when token trails repeated list" in {
    parse(numberToken(3) ++! (numberToken(9)*) ++ numberToken(1))(3, 9, 1) shouldBe Right((Vector(), (3, Vector(9), 1)))
  }

}

class GrammarTestsEntireSequence extends GrammarTestsCommon with GrammarTestHelpersEntireSequence
class GrammarTestsSingleTokens extends GrammarTestsCommon with GrammarTestHelpersSingleTokens
