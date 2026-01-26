package dev.argon.io

import zio.IO

trait ResourceSink[+E, -Res[+_]] {
  def fileName: Option[String]
  def consume[E1 >: E](resource: Res[E1]): IO[E1, Unit]
}
