package dev.argon.io

import java.io.IOException

import dev.argon.io.fileio.FileIO
import dev.argon.stream.builder.{Source, ZStreamSource}
import zio.stream.{Stream, ZStream}
import zio._
import cats.implicits._

import scala.scalajs.js.typedarray.Uint8Array
import scalajs.js.JSConverters._

trait FileIOPlatform {

  val liveNode: FileIO.Service = new NodeIOService()

  val memFSLayer: ZLayer[Any, Nothing, FileIO] =
    ZLayer.fromEffect(
      for {
        fs <- Ref.make[Map[String, Chunk[Byte]]](Map.empty)
      } yield new MemoryIOService(fs)
    )

}
