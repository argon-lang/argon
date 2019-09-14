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

object Program extends App {


  override def run(args: List[String]): ZIO[Environment, Nothing, Int] =
    CommandLineParser.parse(CommandLineArguments.parser)(PlatformHelpers.getCommandLineArgs(args)) match {
      case Some(CommandLineArguments(cmd: CompileCommand[Id])) =>
        runCompilation(cmd)
          .provideSome(PlatformHelpers.ioEnvironment)
          .orDie

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
