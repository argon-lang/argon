package com.mi3software.argon.build.testrunner

import java.io.File

import scala.xml.XML
import scalaz.effect.IO
import scalaz._
import Scalaz._

object TestCaseLoader {

  def findTestCases(dir: File): IO[TestCaseStructure] = for {
    testCaseFiles <- IO { dir.listFiles.sortBy(f => f.getName).toVector }

    subDirCases <- testCaseFiles
      .filterM { f => IO { f.isDirectory } }
      .flatMap {
        _.traverse { f =>
          findTestCases(f).map(f.getName.->)
        }
      }

    fileCases <- testCaseFiles
      .filterM { f => IO { f.isFile && f.getName.endsWith(".xml") } }
      .flatMap { _.traverse(loadTestCase) }

  } yield TestCaseStructure(
    nestedStructures = subDirCases.filter { case (_, nestedCases) => nestedCases.nestedStructures.nonEmpty || nestedCases.tests.nonEmpty },
    tests = fileCases
  )

  def loadTestCase(file: File): IO[TestCase] =
    IO { XML.loadFile(file) }
      .flatMap { elem =>
        TestCase.fromXml(elem)
          .map { _.point[IO] }
          .getOrElse(IO { file.getAbsolutePath }.flatMap { path => IO.throwIO(new Exception(s"Invalid test case ${path}")) })
      }

}
