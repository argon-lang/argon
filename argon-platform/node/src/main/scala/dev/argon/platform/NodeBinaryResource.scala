package dev.argon.platform

import dev.argon.io.*
import zio.*
import zio.stream.*

import scala.scalajs.js.JavaScriptException
import scala.scalajs.js.typedarray.Int8Array


private[platform] final class NodeBinaryResource(path: String) extends BinaryResource[Any, JavaScriptException] {
  override def asBytes: ZStream[Any, JavaScriptException, Byte] =
    ZStream.scoped[Any](
      ZIO.acquireRelease(ZIO.fromPromiseJS { NodeFileSystem.open(path, "r") })
        { fh => ZIO.fromPromiseJS(fh.close()).orDie }
    )
      .refineToOrDie[JavaScriptException]
      .flatMap { fh =>
      ZStream.fromPull[Any, JavaScriptException, Byte](ZIO.succeed(
        ZIO.attempt { new Int8Array(4096) }
          .refineToOrDie[JavaScriptException]
          .asSomeError
          .flatMap { arr =>
            ZIO.fromPromiseJS { fh.read(arr) }
              .refineToOrDie[JavaScriptException]
              .asSomeError
              .flatMap { result =>
                if result.bytesRead == 0 then
                  ZIO.fail(None)
                else
                  ZIO.attempt { Chunk.fromArray(arr.toArray).take(result.bytesRead) }
                    .refineToOrDie[JavaScriptException]
                    .asSomeError
              }
          }
      ))
    }

  override def fileName: Option[String] = Some(path)
}