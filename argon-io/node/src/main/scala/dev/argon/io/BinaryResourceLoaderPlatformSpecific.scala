package dev.argon.io

import java.io.IOException

import zio.*
import zio.stream.*
import scala.scalajs.js.typedarray.Uint8Array

trait BinaryResourceLoaderPlatformSpecific {

  private def fileHandle(path: String, flags: String | Double): URIO[Scope, NodeFileHandle] =
    ZIO.acquireRelease(
      Task.fromPromiseJS(NodeFileSystem.open(path, flags))
    ) { handle =>
      Task.fromPromiseJS(handle.close()).orDie
    }.orDie
  
  def loadFile(path: String): BinaryResource[IOException] =
    BinaryResourceLoader.loadStream(
      ZStream.fromPull[Any, Throwable, Byte](
        for {
          handle <- fileHandle(path, "r")
          buffer <- IO.succeed { new Uint8Array(ZStream.DefaultChunkSize) }
        } yield (
          for {
            readRes <- Task.fromPromiseJS(handle.read(buffer, 0, buffer.length)).asSomeError
            data <-
              if readRes.bytesRead > 0 then
                IO.succeed {
                  val cb = new ChunkBuilder.Byte()
                  cb.sizeHint(readRes.bytesRead)
                  for(i <- 0 until readRes.bytesRead) {
                    cb.addOne(buffer(i).toByte)
                  }
                  cb.result()
                }
              else
                IO.fail(None)
          } yield data
        )
      ).refineToOrDie[IOException]
    )
}
