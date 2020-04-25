package dev.argon.io

import cats.Show
import zio._

import scala.jdk.CollectionConverters._
import scala.jdk.StreamConverters._

final class Path(private[io] val javaPath: java.nio.file.Path) extends AnyVal {
  def root: Option[Path] = Option(javaPath.getRoot).map(new Path(_))

  def fileName: String = Option(javaPath.getFileName).fold("")(pathToString)

  def resolve(other: Path): Path = new Path(javaPath.resolve(other.javaPath))

  def parent: Option[Path] = Option(javaPath.getParent).map(new Path(_))

  def extension: String = {
    val name = fileName
    val index = name.lastIndexOf(".")

    if(index > 1) name.substring(index + 1)
    else ""
  }

  def fileNameWithoutExtension: String = {
    val name = fileName
    val index = name.lastIndexOf(".")

    if(index > 1) name.substring(0, index)
    else name
  }

  def toList: List[Path] = javaPath.asScala.map(new Path(_)).toList

  override def toString: String = pathToString(javaPath)


  @SuppressWarnings(Array("org.wartremover.warts.ToString"))
  private def pathToString(path: java.nio.file.Path): String =
    path.toString

}

object Path {
  @SuppressWarnings(Array("dev.argon.warts.ZioEffect"))
  def of(first: String, more: String*): UIO[Path] =
    IO.effectTotal { new Path(java.nio.file.Path.of(first, more: _*)) }


  implicit val pathShowInstance: Show[Path] =
    _.toString

}
