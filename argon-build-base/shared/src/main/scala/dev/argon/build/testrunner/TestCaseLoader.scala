package dev.argon.build.testrunner

import java.io.IOException

import scala.xml.{Elem, XML}
import cats._
import cats.implicits._
import zio._
import zio.interop.catz.core._
import dev.argon.build._
import dev.argon.io.FileNameUtil
import dev.argon.io.fileio._
import dev.argon.util.{VectorUnCons, XmlParser}

object TestCaseLoader {

  private def loadTestCase(path: String): ZIO[FileIO, Throwable, TestCase] =
    ZIO.accessM[FileIO](_.get.readAllText(path))
      .flatMap(XmlParser.parseString)
      .flatMap { content =>
        IO.fromEither(
          for {
            nameElem <- (content \ "Name").headOption.toRight { new Exception(s"Test case missing name: ${content.toString}") }
            name = nameElem.text

            enabled <- (content \ "@enabled").headOption match {
              case Some(enabledAttr) => enabledAttr.text.toBooleanOption.toRight { new Exception(s"Invalid value for enabled in test case: $name") }
              case None => Right(true)
            }

            testFiles <- (content \ "InputSource").toVector.traverse { inputSourceElem =>
              for {
                nameAttr <- (inputSourceElem \ "@name").headOption.toRight { new Exception(s"Test case file missing name: ${name}") }
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
                .toRight { new Exception(s"Missing expection for test case: ${name}") }


          } yield TestCase(name, enabled, testFiles, expectedResult)
        )
      }

  def loadTestCases(path: String): ZIO[FileIO, Throwable, TestCaseStructure] =
    ZIO.accessM[FileIO](_.get.listDirectory(path).foldM(
      (Seq.empty[(String, TestCaseStructure)], Seq.empty[TestCase])
    ) { case ((dirs, tests), path) =>

      ZIO.ifM(ZIO.accessM[FileIO](_.get.isDirectory(path)))(
        loadTestCases(path).map { subCases => (dirs :+ (FileNameUtil.getBaseName(path) -> subCases), tests) },
        loadTestCase(path).map { testCase => (dirs, tests :+ testCase) }
      )

    })
    .map { case (dirs, tests) =>
      TestCaseStructure(dirs, tests)
    }


}
