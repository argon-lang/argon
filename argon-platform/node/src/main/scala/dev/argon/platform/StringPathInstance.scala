package dev.argon.platform

import dev.argon.io.Path
import cats.implicits._
import zio.{IO, UIO}

private[platform] class StringPathInstance extends Path[String] {
  @SuppressWarnings(Array("dev.argon.warts.ZioEffect"))
  override def pathOf(first: String, more: String*): UIO[String] =
    IO.effectTotal { JSPath.join(first +: more: _*) }

  override def root(path: String): Option[String] = {
    val rootPath = JSPath.parse(path).root

    Option.when(rootPath.nonEmpty)(rootPath)
  }

  override def fileName(path: String): String = JSPath.basename(path)

  override def resolve(path: String)(other: String): String = JSPath.join(path, other)

  override def parent(path: String): Option[String] = {
    val dir = JSPath.dirname(path)
    if(dir === path) None
    else Some(dir)
  }

  override def extension(path: String): String = {
    val name = fileName(path)
    val index = name.lastIndexOf(".")

    if(index > 1) name.substring(index + 1)
    else ""
  }

  override def fileNameWithoutExtension(path: String): String = {
    val name = fileName(path)
    val index = name.lastIndexOf(".")

    if(index > 1) name.substring(0, index)
    else name
  }

  override def segments(path: String): List[String] =
    path.split(Array('/', '\\')).toList

  override def fullPathString(path: String): String = path
}
