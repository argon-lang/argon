package dev.argon.util

import java.io.{File, FileNotFoundException}
import java.nio.file._

import scala.collection.JavaConverters._
import scala.compat.java8.StreamConverters._

object FilenameManip {

  def getExtension(file: File): String = {
    val name = file.getName
    val index = name.lastIndexOf(".")

    if(index > 1) name.substring(index + 1)
    else ""
  }

  def getBasename(file: File): String = {
    val name = file.getName
    val index = name.lastIndexOf(".")

    if(index > 1) name.substring(0, index)
    else name
  }

  @SuppressWarnings(Array("org.wartremover.warts.ToString", "org.wartremover.warts.Equals"))
  def findGlob(baseDir: Path, path: Path): Stream[Path] = {

    def globSegmentMatches(glob: String)(path: Path): Boolean =
      FileSystems.getDefault.getPathMatcher("glob:" + glob).matches(path.getFileName)

    def findGlobImpl(globs: List[Path])(baseDir: Path): Stream[Path] =
      globs match {
        case Nil => Stream(baseDir)
        case _ :: _ if !Files.isDirectory(baseDir) => Stream()

        case head :: tail if head.toString == "**" =>
          Files.list(baseDir)
            .toScala[Stream]
            .flatMap(findGlobImpl(globs)) ++
            findGlobImpl(tail)(baseDir)

        case head :: tail =>
          Files.list(baseDir)
            .toScala[Stream]
            .filter(globSegmentMatches(head.toString))
            .flatMap(findGlobImpl(tail))

      }


    def findTrueBase(baseDir: Path, path: List[Path]): Stream[Path] =
      path match {
        case head :: _ if head.toString contains "*" =>
          if(!Files.isDirectory(baseDir))
            throw new FileNotFoundException(baseDir.toString)
          else
            findGlobImpl(path)(baseDir)

        case head :: tail => findTrueBase(baseDir.resolve(head), tail)
        case Nil => Stream(baseDir)
      }

    val pathRoot = path.getRoot
    findTrueBase(if(pathRoot == null) baseDir.toAbsolutePath else pathRoot, path.asScala.toList)
  }

}
