package dev.argon.io

import java.io.{File, FileNotFoundException, IOException}
import java.nio.file._

import zio._
import zio.stream._
import cats._
import cats.implicits._

import scala.jdk.CollectionConverters._
import scala.jdk.StreamConverters._

object FilenameManip {

  @SuppressWarnings(Array("org.wartremover.warts.ToString"))
  def getExtension(path: Path): String =
    path.asScala.lastOption.map { namePart =>
      val name = namePart.toString
      val index = name.lastIndexOf(".")

      if(index > 1) name.substring(index + 1)
      else ""
    }.getOrElse("")

  def getBasename(file: File): String = {
    val name = file.getName
    val index = name.lastIndexOf(".")

    if(index > 1) name.substring(0, index)
    else name
  }

  @SuppressWarnings(Array("org.wartremover.warts.ToString", "org.wartremover.warts.Equals"))
  def findGlob(baseDir: Path, path: Path): ZStream[FileIO, IOException, Path] = {

    def globSegmentMatches(glob: String)(path: Path): Boolean =
      FileSystems.getDefault.getPathMatcher("glob:" + glob).matches(path.getFileName)

    def findGlobImpl(globs: List[Path])(baseDir: Path): ZStream[FileIO, IOException, Path] =
      globs match {
        case Nil => Stream(baseDir)
        case head :: tail =>
          ZStream.flatten(ZStream.fromEffect(
            ZIO.accessM[FileIO] { _.fileIO.isDirectory(baseDir) }.flatMap {
              case false => IO.succeed(Stream.empty)
              case true if head.toString === "**" =>
                ZIO.access[FileIO] { _.fileIO.listDirectory(baseDir) }
                  .map { dirEntries =>
                    dirEntries.flatMap(findGlobImpl(globs)) ++ findGlobImpl(tail)(baseDir)
                  }

              case true =>
                ZIO.access[FileIO] { _.fileIO.listDirectory(baseDir) }
                  .map { dirEntries =>
                    dirEntries
                      .filter(globSegmentMatches(head.toString))
                      .flatMap(findGlobImpl(tail))
                  }

            }
          ))
      }


    def findTrueBase(baseDir: Path, path: List[Path]): ZStream[FileIO, IOException, Path] =
      path match {
        case head :: _ if head.toString contains "*" =>
          ZStream.fromEffect(ZIO.accessM[FileIO] { _.fileIO.isDirectory(baseDir) }).flatMap {
            case false => Stream.fail(throw new FileNotFoundException(baseDir.toString))
            case true => findGlobImpl(path)(baseDir)
          }

        case head :: tail => findTrueBase(baseDir.resolve(head), tail)
        case Nil => Stream(baseDir)
      }


    val pathRoot = path.getRoot

    ZStream.fromEffect(
      if(pathRoot eq null)
        ZIO.accessM[FileIO] { _.fileIO.getAbsolutePath(baseDir) }
      else
        IO.succeed(pathRoot)
    )
      .flatMap(findTrueBase(_, path.asScala.toList))
  }

}
