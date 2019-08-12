package dev.argon.build.testrunner

import java.io.File

import scala.xml.XML
import cats._
import cats.implicits._
import zio._
import zio.interop.catz._
import dev.argon.build._
import dev.argon.io.FileIO

object TestCaseLoader {

  def findTestCases(dir: File): RIO[FileIO, TestCaseStructure] = for {
    env <- ZIO.environment[FileIO]
    testCaseFilesUnsorted <- env.fileIO.listDirectory(dir.toPath).map { _.toFile }.runCollect
    testCaseFiles = testCaseFilesUnsorted.sortBy(f => f.getName).toVector

    subDirCases <- testCaseFiles
      .filterA { f => env.fileIO.isDirectory(f.toPath) }
      .flatMap {
        _.traverse { f =>
          findTestCases(f).map(f.getName.->)
        }
      }

    fileCases <- testCaseFiles
      .filterA { f => env.fileIO.isDirectory(f.toPath).map { !_ && f.getName.endsWith(".xml") } }
      .flatMap { _.traverse(loadTestCase) }

  } yield TestCaseStructure(
    nestedStructures = subDirCases.filter { case (_, nestedCases) => nestedCases.nestedStructures.nonEmpty || nestedCases.tests.nonEmpty },
    tests = fileCases
  )

  def loadTestCase(file: File): Task[TestCase] =
    IO.effect { XML.loadFile(file) }
      .flatMap { elem =>
        TestCase.fromXml(elem)
          .map { _.pure[IO[Throwable, ?]] }
          .getOrElse(IO.effect { file.getAbsolutePath }.flatMap { path => IO.fail(new Exception(s"Invalid test case ${path}")) })
      }

}
