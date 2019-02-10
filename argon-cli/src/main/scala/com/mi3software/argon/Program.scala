package com.mi3software.argon

import com.mi3software.argon.build.Pipeline
import com.mi3software.argon.build.project.BuildInfo
import scalaz._
import Scalaz._
import scalaz.zio.{ BuildInfo => _, _ }
import scalaz.zio.console._
import scalaz.zio.interop.scalaz72._
import com.mi3software.argon.util.FileOperations
import org.apache.commons.lang3.exception.ExceptionUtils
import scalaz.zio.duration.Duration

object Program extends App {


  override def run(args: List[String]): IO[Nothing, ExitStatus] =
    args match {
      case "compile" :: tail =>
        runCompilation(tail)
          .map { ExitStatus.ExitWhenDone(_, Duration.Infinity) }
          .catchAll { ex =>
            IO.sync {
              ExceptionUtils.printRootCauseStackTrace(ex)
              ExitStatus.ExitNow(1)
            }
          }
      case cmd :: _ => putStrLn(s"Unknown command '$cmd'.").const(ExitStatus.ExitNow(1))
      case _ => putStrLn("Please specify a command.").const(ExitStatus.ExitNow(1))
    }

  private def runCompilation(args: List[String]): IO[Throwable, Int] =
    args match {
      case buildInfoFileName :: Nil =>
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

      case Nil =>
        putStrLn("Please specify the build info file.").const(1)

      case _ :: _ :: _ =>
        putStrLn("Only one file (the build info file) may be specified.").const(1)
    }


}
