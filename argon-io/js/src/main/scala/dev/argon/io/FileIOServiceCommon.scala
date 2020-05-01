package dev.argon.io

import java.io.{ByteArrayOutputStream, IOException}

import dev.argon.stream.builder._
import scalapb.{GeneratedMessage, GeneratedMessageCompanion, Message}
import zio.stream.ZStream
import zio._

import scala.scalajs.js
import scala.scalajs.js.typedarray.Uint8Array
import scala.scalajs.js.JSConverters._
import scala.scalajs.js.|

import dev.argon.io.fileio.FileIO

@SuppressWarnings(Array("dev.argon.warts.ZioEffect"))
trait FileIOServiceCommon extends FileIO.Service with FileIOCommon {
  override def getAbsolutePath(path: Path): IO[IOException, Path] =
    IO.effect { new Path(JSPath.resolve(path.pathName)) }
      .refineOrDie { case e: IOException => e }

}
