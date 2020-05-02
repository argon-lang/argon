package dev.argon.io

import cats.Show
import zio.UIO

trait Path[P] extends Show[P] {
  def pathOf(first: String, more: String*): UIO[P]

  def root(path: P): Option[P]

  def fileName(path: P): String
  def resolve(path: P)(other: P): P
  def parent(path: P): Option[P]
  def extension(path: P): String
  def fileNameWithoutExtension(path: P): String

  def segments(path: P): List[P]

  def fullPathString(path: P): String

  final override def show(t: P): String = fullPathString(t)
}

object Path {

  def apply[P: Path]: Path[P] = implicitly[Path[P]]

  def of[P: Path](first: String, more: String*): UIO[P] = Path[P].pathOf(first, more: _*)

  implicit class PathExtensions[P](val path: P) extends AnyVal {

    def root(implicit pathInstance: Path[P]): Option[P] = Path[P].root(path)

    def fileName(implicit pathInstance: Path[P]): String = Path[P].fileName(path)
    def resolve(other: P)(implicit pathInstance: Path[P]): P = Path[P].resolve(path)(other)
    def parent(implicit pathInstance: Path[P]): Option[P] = Path[P].parent(path)
    def extension(implicit pathInstance: Path[P]): String = Path[P].extension(path)
    def fileNameWithoutExtension(implicit pathInstance: Path[P]): String = Path[P].fileNameWithoutExtension(path)

    def segments(implicit pathInstance: Path[P]): List[P] = Path[P].segments(path)

    def fullPathString(implicit pathInstance: Path[P]): String = Path[P].fullPathString(path)

  }

}
