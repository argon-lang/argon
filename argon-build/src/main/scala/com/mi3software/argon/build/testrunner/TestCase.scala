package com.mi3software.argon.build.testrunner

import scala.collection.immutable._
import scala.xml.Elem
import scalaz.std.option._
import scalaz.std.vector._
import scalaz.syntax.traverse._

final case class TestCase(name: String, sourceCode: Vector[InputSourceData], expectedResult: TestCaseExpectedResult)

sealed trait TestCaseExpectedResult
final case class TestCaseExpectedOutput(text: String) extends TestCaseExpectedResult
final case class TestCaseExpectedError(errorName: String) extends TestCaseExpectedResult

object TestCase {

  def fromXml(elem: Elem): Option[TestCase] = for {
    testName <- (elem \ "Name").headOption
    inputSources <- (elem \ "InputSource")
      .collect { case inputSourceElem: Elem => loadInputSource(inputSourceElem) }
      .toVector
      .sequence

    expectedResult <-
      (elem \ "ExpectedOutput")
        .collectFirst { case outputElem: Elem => TestCaseExpectedOutput(outputElem.text) }
        .orElse {
          (elem \ "ExpectedError")
            .collectFirst { case errorElem: Elem => TestCaseExpectedError(errorElem.text) }
        }

  } yield TestCase(testName.text, inputSources, expectedResult)


  private def loadInputSource(elem: Elem): Option[InputSourceData] = for {
    name <- (elem \ "@name").headOption
  } yield InputSourceData(name.text, elem.text)

}