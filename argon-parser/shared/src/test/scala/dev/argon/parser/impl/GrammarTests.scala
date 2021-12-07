package dev.argon.parser.impl

import dev.argon.grammar.Grammar
import Grammar.Operators._
import scala.language.postfixOps

import zio.NonEmptyChunk
import zio.test._
import zio.test.Assertion._

abstract class GrammarTestsCommon extends DefaultRunnableSpec with GrammarTestHelpers with NumberTokenHelpers {

  def suiteName: String

  private lazy val rightRec: TGrammar[Int] =
    numberToken(0) | numberToken(1) ++ rightRec --> { case (a, b) => a + b }


  override def spec: ZSpec[Environment, Failure] =
    suite(suiteName)(
      suite("Token grammar")(
        test("fail for EOF") {
          assert(parse(numberToken(5))())(isLeft(anything))
        },
        test("fail for wrong token") {
          assert(parse(numberToken(5))(8))(isLeft(anything))
        },
        test("succeed for correct token") {
          assert(parse(numberToken(5))(5))(isRight(equalTo((Vector[TToken](), 5))))
        },
      ),
      suite("Optional grammar")(
        test("succeed for EOF") {
          assert(parse(numberToken(6)?)())(isRight(equalTo((Vector[TToken](), Option.empty[Int]))))
        },
        test("not consume wrong token") {
          assert(parse(numberToken(6)?)(2))(isRight(equalTo((Vector(2), Option.empty[Int]))))
        },
        test("succeed for EOF") {
          assert(parse(numberToken(6)?)(6))(isRight(equalTo((Vector[TToken](), Some(6) : Option[Int]))))
        },
      ),
      suite("Repeated grammar (*)")(
        test("succeed for EOF") {
          assert(parse(numberToken(7).*)())(isRight(equalTo((Vector[TToken](), Vector[Int]()))))
        },
        test("not consume wrong token") {
          assert(parse(numberToken(7).*)(4))(isRight(equalTo((Vector(4), Vector[Int]()))))
        },
        test("succeed for correct token") {
          assert(parse(numberToken(7).*)(7))(isRight(equalTo((Vector[TToken](), Vector(7)))))
        },
        test("succeed for 2 correct tokens") {
          assert(parse(numberToken(7).*)(7, 7))(isRight(equalTo((Vector[TToken](), Vector(7, 7)))))
        },
        test("succeed with trailing token") {
          assert(parse(((numberToken(7) ++ numberToken(8)).*) ++ numberToken(4))(7, 8, 7, 8, 4))(isRight(equalTo((Vector[TToken](), (Vector((7, 8), (7, 8)), 4)))))
        },
      ),
      suite("Repeated grammar (+~)")(
        test("fail for EOF") {
          assert(parse(numberToken(7)+~)())(isLeft(anything))
        },
        test("fail for wrong token") {
          assert(parse(numberToken(7)+~)(4))(isLeft(anything))
        },
        test("succeed for correct token") {
          assert(parse(numberToken(7)+~)(7))(isRight(equalTo((Vector[TToken](), NonEmptyChunk(7)))))
        },
        test("succeed for 2 correct tokens") {
          assert(parse(numberToken(7)+~)(7, 7))(isRight(equalTo((Vector[TToken](), NonEmptyChunk(7, 7)))))
        },
      ),
      suite("Union grammar")(
        test("fail for EOF") {
          assert(parse(numberToken(0) | numberToken(1))())(isLeft(anything))
        },
        test("fail for wrong token") {
          assert(parse(numberToken(0) | numberToken(1))(2))(isLeft(anything))
        },
        test("succeed for left token") {
          assert(parse(numberToken(0) | numberToken(1))(0))(isRight(equalTo((Vector[TToken](), 0))))
        },
        test("succeed for right token") {
          assert(parse(numberToken(0) | numberToken(1))(1))(isRight(equalTo((Vector[TToken](), 1))))
        },
      ),
      suite("Concat grammar")(
        test("fail for EOF") {
          assert(parse(numberToken(4) ++ numberToken(8))())(isLeft(anything))
        },
        test("fail for wrong token") {
          assert(parse(numberToken(4) ++ numberToken(8))(2))(isLeft(anything))
        },
        test("fail for wrong first token") {
          assert(parse(numberToken(4) ++ numberToken(8))(2, 8))(isLeft(anything))
        },
        test("fail for wrong second token") {
          assert(parse(numberToken(4) ++ numberToken(8))(4, 7))(isLeft(anything))
        },
        test("fail for first token") {
          assert(parse(numberToken(4) ++ numberToken(8))(4))(isLeft(anything))
        },
        test("fail for second token") {
          assert(parse(numberToken(4) ++ numberToken(8))(8))(isLeft(anything))
        },
        test("succeed for first and second token") {
          assert(parse(numberToken(4) ++ numberToken(8))(4, 8))(isRight(equalTo((Vector[TToken](), (4, 8)))))
        },
      ),
      suite("Right recursive grammar")(
        test("fail for EOF") {
          assert(parse(rightRec)())(isLeft(anything))
        },
        test("fail for wrong token") {
          assert(parse(rightRec)(2))(isLeft(anything))
        },
        test("fail for EOF") {
          assert(parse(rightRec)(1))(isLeft(anything))
        },
        test("succeed for end token") {
          assert(parse(rightRec)(0))(isRight(equalTo((Vector[TToken](), 0))))
        },
        test("succeed for end token") {
          assert(parse(rightRec)(1, 0))(isRight(equalTo((Vector[TToken](), 1))))
        },
        test("succeed for end token") {
          assert(parse(rightRec)(0, 0))(isRight(equalTo((Vector[TToken](0), 0))))
        },
        test("succeed for prefix prefix end") {
          assert(parse(rightRec)(1, 1, 0))(isRight(equalTo((Vector[TToken](), 2))))
        },
      ),
      suite("Strict grammar")(
        test("succeed when first option fails") {
          assert(parse(numberToken(3) ++! (numberToken(9) | numberToken(1)))(3, 1))(isRight(equalTo((Vector[TToken](), (3, 1)))))
        },
        test("succeed when token trails repeated list") {
          assert(parse(numberToken(3) ++! (numberToken(9)*) ++ numberToken(1))(3, 9, 1))(isRight(equalTo((Vector[TToken](), (3, Vector(9), 1)))))
        },
      )
    )
}

object GrammarTestsEntireSequence extends GrammarTestsCommon with GrammarTestHelpersEntireSequence {
  override def suiteName: String = "Grammar parsing with entire sequence"
}

object GrammarTestsSingleTokens extends GrammarTestsCommon with GrammarTestHelpersSingleTokens {
  override def suiteName: String = "Grammar parsing with single tokens"
}

