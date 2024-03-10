package dev.argon.io

import dev.argon.io.*
import zio.*
import zio.stream.Stream

import java.io.IOException

trait PathUtil {
  def exists(path: PathLike): IO[IOException, Boolean]
  def dirname(path: PathLike): UIO[PathLike]
  def listDirectory(path: PathLike): Stream[IOException, PathLike]
  def binaryResource(path: PathLike): BinaryResource[IOException]
  def resourceLayer(baseDir: PathLike): ULayer[ResourceReader & ResourceWriter]
}

object PathUtil extends PathUtilPlatformSpecific
