package dev.argon.io

import zio.*
import zio.stream.*

trait ZipFile {
  def entry(path: String): UIO[Option[ZipEntry]]
  def entries: UStream[ZipEntry]
}

trait ZipEntry {
  def path: String
  def read: UStream[Byte]
}
