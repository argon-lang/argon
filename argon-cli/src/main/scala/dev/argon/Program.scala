package dev.argon

import dev.argon.build.Pipeline
import dev.argon.build.project.BuildInfo
import cats._
import cats.implicits._
import zio.{ BuildInfo => _, _ }
import zio.console._
import zio.interop.catz._
import dev.argon.util.FileOperations
import org.apache.commons.lang3.exception.ExceptionUtils
import shapeless.{ BuildInfo => _, Id => _, _ }
import CommandLineParser.Implicits._

object Program extends App {


  override def run(args: List[String]): ZIO[Environment, Nothing, Int] =
    CommandLineParser.parse(CommandLineArguments.parser)(args) match {
      case Some(CommandLineArguments(cmd: CompileCommand[Id])) =>
        runCompilation(cmd)
          .catchAll { ex =>
            IO.effectTotal {
              ExceptionUtils.printRootCauseStackTrace(ex)
              1
            }
          }

      case None =>
        for {
          _ <- putStrLn("Invalid arguments")
        } yield 1
    }

  private def runCompilation(args: CompileCommand[Id]): ZIO[Environment, Throwable, Int] =
    args match {
      case CompileCommand(buildInfoFileName) =>
        FileOperations.fileFromName(buildInfoFileName)
          .flatMap(BuildInfo.loadFile)
          .flatMap {
            case Some(buildInfos) => buildInfos.foldLeftM(0) { (exitCode, buildInfo) =>
              Pipeline.run(buildInfo).map { exitCode2 =>
                if(exitCode === 0) exitCode2
                else exitCode
              }
            }
            case None => putStrLn("Could not load build info file.").const(1)
          }
    }


}
