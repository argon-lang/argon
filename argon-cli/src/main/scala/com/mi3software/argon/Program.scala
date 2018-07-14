package com.mi3software.argon

import com.mi3software.argon.pipeline.Pipeline
import scalaz.effect.{IO, SafeApp}
import scalaz._
import com.mi3software.argon.util.FileOperations

import com.mi3software.argon.builder.BuildInfo

object Program extends SafeApp {
  override def runl(args: List[String]): IO[Unit] = args match {
    case "compile" :: tail => runCompilation(tail)
    case cmd :: _ => IO.putStrLn(s"Unknown command '$cmd'.")
    case _ => IO.putStrLn("Please specify a command.")
  }

  private def runCompilation(args: List[String]): IO[Unit] =
    args match {
      case buildInfoFileName :: Nil =>
        FileOperations.fileFromName(buildInfoFileName)
          .flatMap(BuildInfo.loadFile)
          .flatMap {
            case Some(buildInfo) => Pipeline.run(buildInfo)
            case None =>
              IO.putStrLn("Could not load build info file.")
          }

      case Nil =>
        IO.putStrLn("Please specify the build info file.")

      case _ :: _ :: _ =>
        IO.putStrLn("Only one file (the build info file) may be specified.")
    }


}
