package dev.argon.io

import dev.argon.io.*
import dev.argon.io.jstypes.node.NodeFileSystem
import dev.argon.util.AsyncIterableTools
import zio.*
import zio.stream.*

import java.io.IOException
import scala.scalajs.js.JavaScriptException
import scala.scalajs.js.typedarray.{Int8Array, Uint8Array}


private[io] final class NodeBinaryResource(path: String) extends BinaryResource[Any, IOException] {
  override def asBytes: ZStream[Any, IOException, Byte] =
    ZStream.scoped[Any](
      ZIO.acquireRelease(ZIO.fromPromiseJS { NodeFileSystem.open(path, "r") })
        { fh => ZIO.fromPromiseJS(fh.close()).orDie }
    )
      .flatMap { fh =>
        AsyncIterableTools.asyncIterableToZStreamRaw(fh.readableWebStream())
          .map(b => uint8ArrayToChunk(new Uint8Array(b)))
          .flattenChunks
      }
      .refineOrDie {
        case ex: JavaScriptException => IOException(ex)
      }

  override def fileName: Option[String] = Some(path)
}
