package dev.argon.util

import java.io.{File, FileNotFoundException}
import java.nio.file._

import scala.jdk.CollectionConverters._
import scala.jdk.StreamConverters._

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
  def findGlob(baseDir: Path, path: Path): LazyList[Path] = {

    def globSegmentMatches(glob: String)(path: Path): Boolean =
      FileSystems.getDefault.getPathMatcher("glob:" + glob).matches(path.getFileName)

    def findGlobImpl(globs: List[Path])(baseDir: Path): LazyList[Path] =
      globs match {
        case Nil => LazyList(baseDir)
        case _ :: _ if !Files.isDirectory(baseDir) => LazyList()

        case head :: tail if head.toString == "**" =>
          Files.list(baseDir)
            .toScala(LazyList)
            .flatMap(findGlobImpl(globs)) ++
            findGlobImpl(tail)(baseDir)

        case head :: tail =>
          Files.list(baseDir)
            .toScala(LazyList)
            .filter(globSegmentMatches(head.toString))
            .flatMap(findGlobImpl(tail))

      }


    def findTrueBase(baseDir: Path, path: List[Path]): LazyList[Path] =
      path match {
        case head :: _ if head.toString contains "*" =>
          if(!Files.isDirectory(baseDir))
            throw new FileNotFoundException(baseDir.toString)
          else
            findGlobImpl(path)(baseDir)

        case head :: tail => findTrueBase(baseDir.resolve(head), tail)
        case Nil => LazyList(baseDir)
      }

    val pathRoot = path.getRoot
    findTrueBase(if(pathRoot == null) baseDir.toAbsolutePath else pathRoot, path.asScala.toList)
  }

}
