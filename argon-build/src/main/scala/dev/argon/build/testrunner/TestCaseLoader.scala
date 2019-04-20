package dev.argon.build.testrunner

import java.io.File

import scala.xml.XML
import scalaz._
import Scalaz._
import scalaz.zio._
import scalaz.zio.interop.scalaz72._

object TestCaseLoader {

  def findTestCases(dir: File): Task[TestCaseStructure] = for {
    testCaseFiles <- IO.effect { dir.listFiles.sortBy(f => f.getName).toVector }

    subDirCases <- testCaseFiles
      .filterM { f => IO.effect { f.isDirectory } }
      .flatMap {
        _.traverse { f =>
          findTestCases(f).map(f.getName.->)
        }
      }

    fileCases <- testCaseFiles
      .filterM { f => IO.effect { f.isFile && f.getName.endsWith(".xml") } }
      .flatMap { _.traverse(loadTestCase) }

  } yield TestCaseStructure(
    nestedStructures = subDirCases.filter { case (_, nestedCases) => nestedCases.nestedStructures.nonEmpty || nestedCases.tests.nonEmpty },
    tests = fileCases
  )

  def loadTestCase(file: File): Task[TestCase] =
    IO.effect { XML.loadFile(file) }
      .flatMap { elem =>
        TestCase.fromXml(elem)
          .map { _.point[IO[Throwable, ?]] }
          .getOrElse(IO.effect { file.getAbsolutePath }.flatMap { path => IO.fail(new Exception(s"Invalid test case ${path}")) })
      }

}
