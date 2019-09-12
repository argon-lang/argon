package dev.argon.io

import cats.Show
import zio._

import scala.jdk.CollectionConverters._
import scala.jdk.StreamConverters._

final class Path(private[io] val pathName: String) extends AnyVal {
  def root: Option[Path] = ???

  def fileName: String = JSPath.basename(pathName)

  def resolve(other: Path): Path = new Path(JSPath.join(pathName, other.pathName))

  def parent: Option[Path] = Some(new Path(JSPath.dirname(pathName)))

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

  def toList: List[Path] = pathName.split(Array('/', '\\')).map(new Path(_)).toList

  override def toString: String = pathName
}

object Path {
  def of(first: String, more: String*): UIO[Path] =
    IO.effectTotal { new Path(JSPath.join(first +: more: _*)) }


  implicit val pathShowInstance: Show[Path] =
    _.toString

}
