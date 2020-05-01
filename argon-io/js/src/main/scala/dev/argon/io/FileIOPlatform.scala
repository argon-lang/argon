package dev.argon.io

import java.io.IOException

import dev.argon.io.fileio.FileIO
import dev.argon.stream.builder.{Source, ZStreamSource}
import zio.stream.{Stream, ZStream}
import zio._
import cats.implicits._


trait FileIOPlatform {

  val liveNode: FileIO.Service = new NodeIOService()

}
