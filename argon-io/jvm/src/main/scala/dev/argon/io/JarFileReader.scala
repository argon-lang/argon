package dev.argon.io

import zio.ZIO
import java.util.jar.Manifest

trait JarFileReader[-R, +E] extends ZipFileReader[R, E] {
  def jarName: ZIO[R, E, String]
  def manifest: ZIO[R, E, Option[Manifest]]
}
