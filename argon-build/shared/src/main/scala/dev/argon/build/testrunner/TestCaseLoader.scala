package dev.argon.build.testrunner

import java.nio.file.Path

import scala.xml.{Elem, XML}
import cats._
import cats.implicits._
import zio._
import zio.interop.catz._
import dev.argon.build._
import dev.argon.io.{FileIO, FilenameManip}

object TestCaseLoader {

  def findTestCases(testCases: Seq[(Seq[String], TestCase)]): TestCaseStructure = {
    val groupedTestCases = testCases
      .map {
        case (head +: tail, testCase) => (Some(head), tail, testCase)
        case (Seq(), testCase) => (None, Seq(), testCase)
      }
      .groupMap {
        case (key, _, _) => key
      } {
        case (_, path, testCase) => (path, testCase)
      }

    val subDirCases = groupedTestCases
      .collect {
        case (Some(k), v) => (k, findTestCases(v))
      }
      .toSeq
      .sortBy { case (k, _) => k }

    val fileCases = groupedTestCases
      .getOrElse(None, Seq())
      .map { case (_, testCase) => testCase }
      .sortBy { _.name }

    TestCaseStructure(subDirCases, fileCases)
  }


}
