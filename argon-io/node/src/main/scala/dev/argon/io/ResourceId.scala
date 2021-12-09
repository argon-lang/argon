package dev.argon.io

import zio.stream.*
import zio.*
import scala.scalajs.js.typedarray.Uint8Array

trait ResourceId {
  def asStream: ZStream[Any, Nothing, Byte]
  def fileName: Option[String] = None
}

object ResourceId {

  def fromFileName(file: String): ResourceId =
    new ResourceId {

      override def asStream: UStream[Byte] =
        readByteChunks.map { arr =>
          val chunkBuilder = ChunkBuilder.make[Byte]()
          chunkBuilder.sizeHint(arr)
          arr.foreach { b =>
            chunkBuilder.addOne(b.toByte)
          }

          chunkBuilder.result()
        }
          .flattenChunks

      override def fileName: Option[String] = Some(file)

      private def readByteChunks: UStream[Uint8Array] =
        ZStream.fromPull(
          ZManaged.make(
            ZIO.fromPromiseJS(NodeFileSystem.open(file, "r")).orDie
          )(handle => ZIO.fromPromiseJS(handle.close()).orDie)
            .map { handle =>
              for {
                buffer <- IO.succeed { new Uint8Array(4096) }
                result <- ZIO.fromPromiseJS(handle.read(buffer, 0, buffer.length, null)).orDie
              } yield Chunk(buffer.subarray(0, result.bytesRead))
            }
        )

    }

}
