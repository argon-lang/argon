package dev.argon.build.testrunner

import java.io.File
import java.nio.file.Path

import scala.xml.XML
import cats._
import cats.implicits._
import zio._
import zio.interop.catz._
import dev.argon.build._
import dev.argon.io.{FileIO, FilenameManip}

object TestCaseLoader {

  def findTestCases(dir: Path): RIO[FileIO, TestCaseStructure] = for {
    env <- ZIO.environment[FileIO]
    testCaseFilesUnsorted <- env.fileIO.listDirectory(dir).runCollect
    testCaseFiles = testCaseFilesUnsorted.sortBy(FilenameManip.getFileName).toVector

    subDirCases <- testCaseFiles
      .filterA(env.fileIO.isDirectory)
      .flatMap {
        _.traverse { f =>
          findTestCases(f).map(FilenameManip.getFileName(f).->)
        }
      }

     fileCases <- testCaseFiles
      .filterA { f => env.fileIO.isDirectory(f).map { !_ && FilenameManip.getFileName(f).endsWith(".xml") } }
      .flatMap { _.traverse(loadTestCase) }

  } yield TestCaseStructure(
    nestedStructures = subDirCases.filter { case (_, nestedCases) => nestedCases.nestedStructures.nonEmpty || nestedCases.tests.nonEmpty },
    tests = fileCases
  )

  def loadTestCase(path: Path): Task[TestCase] =
    IO.effect { XML.loadFile(path.toFile) }
      .flatMap { elem =>
        TestCase.fromXml(elem)
          .map { _.pure[IO[Throwable, ?]] }
          .getOrElse(IO.effect { path.toAbsolutePath }.flatMap { path => IO.fail(new Exception(s"Invalid test case ${FilenameManip.pathToString(path)}")) })
      }

}
