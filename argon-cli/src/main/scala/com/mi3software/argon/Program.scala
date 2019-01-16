package com.mi3software.argon

import com.mi3software.argon.build.{BuildInfo, Pipeline}
import scalaz._
import scalaz.zio.{ BuildInfo => _, _ }
import scalaz.zio.console._
import com.mi3software.argon.util.FileOperations
import org.apache.commons.lang3.exception.ExceptionUtils
import scalaz.zio.duration.Duration

object Program extends App {


  override def run(args: List[String]): IO[Nothing, ExitStatus] =
    args match {
      case "compile" :: tail =>
        runCompilation(tail)
          .const(ExitStatus.ExitWhenDone(0, Duration.Infinity))
          .catchAll { ex =>
            IO.sync {
              ExceptionUtils.printRootCauseStackTrace(ex)
              ExitStatus.ExitNow(1)
            }
          }
      case cmd :: _ => putStrLn(s"Unknown command '$cmd'.").const(ExitStatus.ExitNow(1))
      case _ => putStrLn("Please specify a command.").const(ExitStatus.ExitNow(1))
    }

  private def runCompilation(args: List[String]): IO[Throwable, Unit] =
    args match {
      case buildInfoFileName :: Nil =>
        FileOperations.fileFromName(buildInfoFileName)
          .flatMap(BuildInfo.loadFile)
          .flatMap {
            case Some(buildInfo) => Pipeline.run(buildInfo)
            case None => putStrLn("Could not load build info file.")
          }

      case Nil =>
        putStrLn("Please specify the build info file.")

      case _ :: _ :: _ =>
        putStrLn("Only one file (the build info file) may be specified.")
    }


}
