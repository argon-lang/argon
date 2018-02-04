package com.mi3software.argon.parser.impl

import com.mi3software.argon.parser.GrammarError
import com.mi3software.argon.util.{FilePosition, SourceLocation, WithSource}
import org.scalatest.{FlatSpec, Matchers}

import scalaz._
import Grammar.Operators._

import scala.language.postfixOps

class GrammarTests extends FlatSpec with Matchers {

  type TGrammar = Grammar[Int, GrammarError[Int, String], Int]

  private implicit val errorFactory = new Grammar.ErrorFactory[Int, String, GrammarError[Int, String]] {
    override def createError(error: GrammarError[Int, String]): GrammarError[Int, String] = error

    override def errorEndLocationOrder: Order[GrammarError[Int, String]] =
      (a, b) => implicitly[Order[FilePosition]].order(a.location.end, b.location.end)
  }

  private def numberToken(n: Int): TGrammar = Grammar.matcher(n.toString, m => Some(m).filter(_ === n))

  private val pos = FilePosition(0, 0)

  private def ws[T](t: T) = WithSource(t, SourceLocation.empty)

  "A token grammar" should "fail for EOF" in {
    numberToken(5).endOfInput(pos) should matchPattern { case -\/(_) => }
  }

  it should "fail for wrong token" in {
    numberToken(5).derive(ws(8)).endOfInput(pos) should matchPattern { case -\/(_) => }
  }

  it should "succeed for correct token" in {
    numberToken(5).derive(ws(5)).endOfInput(pos) should matchPattern { case \/-(NonEmptyList(WithSource(5, _), INil())) => }
  }

  "An optional token grammar" should "succeed for EOF" in {
    (numberToken(6)?).endOfInput(pos) should matchPattern { case \/-(NonEmptyList(WithSource(None, _), _)) => }
  }

  it should "fail for wrong token" in {
    (numberToken(6)?).derive(ws(2)).endOfInput(pos) should matchPattern { case -\/(_) => }
  }

  it should "succeed for correct token" in {
    (numberToken(6)?).derive(ws(6)).endOfInput(pos) should matchPattern { case \/-(NonEmptyList(WithSource(Some(6), _), INil())) => }
  }

  "An repeated (*) token grammar" should "succeed for EOF" in {
    (numberToken(7)*).endOfInput(pos) should matchPattern { case \/-(NonEmptyList(WithSource(INil(), _), INil())) => }
  }

  it should "fail for wrong token" in {
    (numberToken(7)*).derive(ws(4)).endOfInput(pos) should matchPattern { case -\/(_) => }
  }

  it should "succeed for correct token" in {
    (numberToken(7)*).derive(ws(7)).endOfInput(pos) should matchPattern { case \/-(NonEmptyList(WithSource(ICons(7, INil()), _), INil())) => }
  }

  it should "succeed for 2 correct tokens" in {
    val g = numberToken(7)*
    val a = g.derive(ws(7)).derive(ws(7)).endOfInput(pos)
    a should matchPattern { case \/-(NonEmptyList(WithSource(ICons(7, ICons(7, INil())), _), INil())) => }
  }

  "An repeated (+~) token grammar" should "fail for EOF" in {
    (numberToken(7)+~).endOfInput(pos) should matchPattern { case -\/(_) => }
  }

  it should "fail for wrong token" in {
    (numberToken(7)*).derive(ws(4)).endOfInput(pos) should matchPattern { case -\/(_) => }
  }

  it should "succeed for correct token" in {
    (numberToken(7)*).derive(ws(7)).endOfInput(pos) should matchPattern { case \/-(NonEmptyList(WithSource(ICons(7, INil()), _), INil())) => }
  }

  it should "succeed for 2 correct tokens" in {
    val g = numberToken(7)*
    val a = g.derive(ws(7)).derive(ws(7)).endOfInput(pos)
    a should matchPattern { case \/-(NonEmptyList(WithSource(ICons(7, ICons(7, INil())), _), INil())) => }
  }

  "An union grammar" should "fail for EOF" in {
    (numberToken(0) | numberToken(1)).endOfInput(pos) should matchPattern { case -\/(_) => }
  }

  it should "fail for wrong token" in {
    (numberToken(0) | numberToken(1)).derive(ws(2)).endOfInput(pos) should matchPattern { case -\/(_) => }
  }

  it should "succeed for left token" in {
    (numberToken(0) | numberToken(1)).derive(ws(0)).endOfInput(pos) should matchPattern { case \/-(NonEmptyList(WithSource(0, _), INil())) => }
  }

  it should "succeed for right token" in {
    (numberToken(0) | numberToken(1)).derive(ws(1)).endOfInput(pos) should matchPattern { case \/-(NonEmptyList(WithSource(1, _), INil())) => }
  }

  "An concat grammar" should "fail for EOF" in {
    (numberToken(4) ++ numberToken(8)).endOfInput(pos) should matchPattern { case -\/(_) => }
  }

  it should "fail for wrong token" in {
    (numberToken(4) ++ numberToken(8)).derive(ws(2)).endOfInput(pos) should matchPattern { case -\/(_) => }
  }

  it should "fail for wrong first token" in {
    (numberToken(4) ++ numberToken(8)).derive(ws(2)).derive(ws(8)).endOfInput(pos) should matchPattern { case -\/(_) => }
  }

  it should "fail for wrong second token" in {
    (numberToken(4) ++ numberToken(8)).derive(ws(4)).derive(ws(7)).endOfInput(pos) should matchPattern { case -\/(_) => }
  }

  it should "fail for first token" in {
    (numberToken(4) ++ numberToken(8)).derive(ws(4)).endOfInput(pos) should matchPattern { case -\/(_) => }
  }

  it should "fail for second token" in {
    (numberToken(4) ++ numberToken(8)).derive(ws(8)).endOfInput(pos) should matchPattern { case -\/(_) => }
  }

  it should "succeed for first and second token" in {
    val a = (numberToken(4) ++ numberToken(8)).derive(ws(4)).derive(ws(8)).endOfInput(pos)
    a should matchPattern { case \/-(NonEmptyList(WithSource((4, 8), _), INil())) => }
  }

  private lazy val leftRec: TGrammar =
    numberToken(0) | leftRec ++ numberToken(1) --> { case (a, b) => a + b }

  "An left recursive grammar" should "fail for EOF" in {
    leftRec.endOfInput(pos) should matchPattern { case -\/(_) => }
  }

  it should "fail for wrong token" in {
    leftRec.derive(ws(2)).endOfInput(pos) should matchPattern { case -\/(_) => }
  }

  it should "fail for tail token" in {
    leftRec.derive(ws(1)).endOfInput(pos) should matchPattern { case -\/(_) => }
  }

  it should "succeed for head token" in {
    leftRec.derive(ws(0)).endOfInput(pos) should matchPattern { case \/-(NonEmptyList(WithSource(0, _), INil())) => }
  }

  it should "succeed for head tail" in {
    leftRec.derive(ws(0)).derive(ws(1)).endOfInput(pos) should matchPattern { case \/-(NonEmptyList(WithSource(1, _), INil())) => }
  }

  it should "fail for head head" in {
    leftRec.derive(ws(0)).derive(ws(0)).endOfInput(pos) should matchPattern { case -\/(_) => }
  }

  it should "succeed for head tail tail" in {
    leftRec.derive(ws(0)).derive(ws(1)).derive(ws(1)).endOfInput(pos) should matchPattern { case \/-(NonEmptyList(WithSource(2, _), INil())) => }
  }

  private lazy val rightRec: TGrammar =
    numberToken(0) | numberToken(1) ++ rightRec --> { case (a, b) => a + b }

  "An right recursive grammar" should "fail for EOF" in {
    rightRec.endOfInput(pos) should matchPattern { case -\/(_) => }
  }

  it should "fail for wrong token" in {
    rightRec.derive(ws(2)).endOfInput(pos) should matchPattern { case -\/(_) => }
  }

  it should "fail for prefix token" in {
    rightRec.derive(ws(1)).endOfInput(pos) should matchPattern { case -\/(_) => }
  }

  it should "succeed for end token" in {
    rightRec.derive(ws(0)).endOfInput(pos) should matchPattern { case \/-(NonEmptyList(WithSource(0, _), INil())) => }
  }

  it should "succeed for prefix end" in {
    rightRec.derive(ws(1)).derive(ws(0)).endOfInput(pos) should matchPattern { case \/-(NonEmptyList(WithSource(1, _), INil())) => }
  }

  it should "fail for end end" in {
    rightRec.derive(ws(0)).derive(ws(0)).endOfInput(pos) should matchPattern { case -\/(_) => }
  }

  it should "succeed for prefix prefix end" in {
    rightRec.derive(ws(1)).derive(ws(1)).derive(ws(0)).endOfInput(pos) should matchPattern { case \/-(NonEmptyList(WithSource(2, _), INil())) => }
  }

}
