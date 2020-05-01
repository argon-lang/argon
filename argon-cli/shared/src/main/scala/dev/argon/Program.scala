package dev.argon

import dev.argon.build.{BuildEnvironment, Pipeline}
import dev.argon.build.project.BuildInfo
import cats._
import cats.implicits._
import zio.{BuildInfo => _, _}
import zio.console._
import zio.interop.catz._
import shapeless.{BuildInfo => _, Id => _, Path => _, _}
import CommandLineParser.Implicits._
import dev.argon.io.Path
import dev.argon.io.fileio.{FileIO, FileIOLite}

object Program extends PlatformApp {


  override def run2(args: List[String]): ZIO[ZEnv with FileIO with FileIOLite, Nothing, Int] =
    CommandLineParser.parse(CommandLineArguments.parser)(args) match {
      case Some(CommandLineArguments(cmd: CompileCommand[Id])) =>
        runCompilation(cmd).orDie

      case None =>
        for {
          _ <- putStrLn("Invalid arguments")
        } yield 1
    }

  private def runCompilation(args: CompileCommand[Id]): ZIO[BuildEnvironment with Console, Throwable, Int] =
    args match {
      case CompileCommand(buildInfoFileName) =>
        Path.of(buildInfoFileName)
          .flatMap(BuildInfo.loadFile)
          .flatMap {
            case Some(buildInfos) => buildInfos.foldLeftM(0) { (exitCode, buildInfo) =>
              Pipeline.run(buildInfo).map { exitCode2 =>
                if(exitCode === 0) exitCode2
                else exitCode
              }
            }
            case None => putStrLn("Could not load build info file.").as(1)
          }
    }


}
