package dev.argon.io

import zio.*
import zio.stream.*

trait ZipFile[-R, +E] {
  def entry(path: String): ZIO[R, E, Option[ZipEntry[R, E]]]
  def entries: ZStream[R, E, ZipEntry[R, E]]
}

trait ZipEntry[-R, +E] {
  def path: String
  def read: ZStream[R, E, Byte]
}
