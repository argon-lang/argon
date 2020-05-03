package dev.argon.platform

import dev.argon.io.Path
import java.nio.file.{Path => JPath}

import zio._

import scala.jdk.CollectionConverters._

private[platform] class JavaPathInstance extends Path[JPath] {
  @SuppressWarnings(Array("dev.argon.warts.ZioEffect"))
  override def pathOf(first: String, more: String*): UIO[JPath] =
    IO.effectTotal { JPath.of(first, more: _*) }

  override def root(path: JPath): Option[JPath] = Option(path.getRoot)

  override def fileName(path: JPath): String = Option(path.getFileName).fold("")(fullPathString)

  override def resolve(path: JPath)(other: JPath): JPath = path.resolve(other)

  override def parent(path: JPath): Option[JPath] = Option(path.getParent)

  override def extension(path: JPath): String = {
    val name = fileName(path)
    val index = name.lastIndexOf(".")

    if(index > 1) name.substring(index + 1)
    else ""
  }

  override def fileNameWithoutExtension(path: JPath): String = {
    val name = fileName(path)
    val index = name.lastIndexOf(".")

    if(index > 1) name.substring(0, index)
    else name
  }

  override def segments(path: JPath): List[JPath] =
    path.asScala.toList

  @SuppressWarnings(Array("org.wartremover.warts.ToString"))
  override def fullPathString(path: JPath): String = path.toString

}
