package dev.argon.io

import java.io.{FileNotFoundException, IOException}
import java.nio.file.FileSystems

import zio._
import zio.stream._
import cats._
import cats.implicits._

import scala.jdk.CollectionConverters._
import scala.jdk.StreamConverters._
import scala.util.matching.Regex

import dev.argon.io.fileio.FileIO
import dev.argon.io.Path.PathExtensions

object FilenameManip {

  @SuppressWarnings(Array("org.wartremover.warts.ToString", "org.wartremover.warts.Equals"))
  def findGlob[P : Path: Tagged](path: P): ZStream[FileIO[P], IOException, P] = {

    def globSegmentMatches(glob: String)(path: P): Boolean =
      glob.split("\\*").map(Regex.quote).mkString(".*").r.matches(path.toString)

    def findGlobImpl(globs: List[P])(baseDir: P): ZStream[FileIO[P], IOException, P] =
      globs match {
        case Nil => Stream(baseDir)
        case head :: tail =>
          ZStream.flatten(ZStream.fromEffect(
            ZIO.accessM[FileIO[P]] { _.get.isDirectory(baseDir) }.flatMap {
              case false => IO.succeed(Stream.empty)
              case true if head.toString === "**" =>
                ZIO.access[FileIO[P]] { _.get.listDirectory(baseDir) }
                  .map { dirEntries =>
                    dirEntries.flatMap(findGlobImpl(globs)) ++ findGlobImpl(tail)(baseDir)
                  }

              case true =>
                ZIO.access[FileIO[P]] { _.get.listDirectory(baseDir) }
                  .map { dirEntries =>
                    dirEntries
                      .filter(globSegmentMatches(head.toString))
                      .flatMap(findGlobImpl(tail))
                  }

            }
          ))
      }


    def findTrueBase(baseDir: P, path: List[P]): ZStream[FileIO[P], IOException, P] =
      path match {
        case head :: _ if head.toString contains "*" =>
          ZStream.fromEffect(ZIO.accessM[FileIO[P]] { _.get.isDirectory(baseDir) }).flatMap {
            case false => Stream.fail(throw new FileNotFoundException(baseDir.toString))
            case true => findGlobImpl(path)(baseDir)
          }

        case head :: tail => findTrueBase(baseDir.resolve(head), tail)
        case Nil => Stream(baseDir)
      }


    ZStream.fromEffect(
      path.root match {
        case Some(pathRoot) => IO.succeed(pathRoot)
        case None => Path.of[P](".").flatMap { rootPath => ZIO.accessM[FileIO[P]] { _.get.getAbsolutePath(rootPath) } }
      }
    )
      .flatMap(findTrueBase(_, path.segments))
  }

}
