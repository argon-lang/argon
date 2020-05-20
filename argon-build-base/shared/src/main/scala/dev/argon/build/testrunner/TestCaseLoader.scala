package dev.argon.build.testrunner

import java.io.IOException

import scala.xml.{Elem, XML}
import cats._
import cats.implicits._
import zio._
import zio.interop.catz.core._
import dev.argon.build._
import dev.argon.io.Path
import dev.argon.io.Path.PathExtensions
import dev.argon.io.fileio._
import dev.argon.util.XmlParser

object TestCaseLoader {

  private def loadTestCase[P: Path : Tag](path: P): ZIO[FileIO[P], Throwable, TestCase] =
    ZIO.accessM[FileIO[P]](_.get.readAllText(path))
      .flatMap(XmlParser.parseString)
      .flatMap { content =>
        IO.fromEither(
          (
            for {
              nameElem <- (content \ "Name").headOption
              name = nameElem.text

              testFiles <- (content \ "InputSource").toVector.traverse { inputSourceElem =>
                for {
                  nameAttr <- (inputSourceElem \ "@name").headOption
                  fileName = nameAttr.text
                  fileData = inputSourceElem.text
                } yield InputSourceData(fileName, fileData)
              }

              expectedResult <-
                (content \ "ExpectedOutput")
                  .collectFirst { case outputElem: Elem => TestCaseExpectedOutput(outputElem.text) }
                  .orElse {
                    (content \ "ExpectedError")
                      .collectFirst { case errorElem: Elem => TestCaseExpectedError(errorElem.text) }
                  }


            } yield TestCase(name, testFiles.toVector, expectedResult)
            ).toRight { new Exception(s"Invalid test case: ${content.toString}") }
        )
      }

  def loadTestCases[P: Path : Tag](path: P): ZIO[FileIO[P], Throwable, TestCaseStructure] =
    ZIO.accessM[FileIO[P]](_.get.listDirectory(path).foldM[FileIO[P], Throwable, P, (Seq[(String, TestCaseStructure)], Seq[TestCase])](
      (Seq.empty[(String, TestCaseStructure)], Seq.empty[TestCase])
    ) { case ((dirs, tests), path) =>

      ZIO.ifM(ZIO.accessM[FileIO[P]](_.get.isDirectory(path)))(
        loadTestCases(path).map { subCases => (dirs :+ (path.fileName -> subCases), tests) },
        loadTestCase(path).map { testCase => (dirs, tests :+ testCase) }
      )

    })
    .map { case (dirs, tests) =>
      TestCaseStructure(dirs, tests)
    }

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
