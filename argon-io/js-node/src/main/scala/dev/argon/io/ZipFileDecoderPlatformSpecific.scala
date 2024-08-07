package dev.argon.io

import dev.argon.util.*
import zio.*
import zio.stream.*

import java.io.IOException
import scala.scalajs.js.typedarray.{Int8Array, Uint8Array}
import dev.argon.io.jstypes.jszip.JSZip
import dev.argon.io.jstypes.jszip.JSZip.JSZipObject

final class ZipFileDecoderPlatformSpecific extends BinaryResourceDecoder[ZipFileResource, Any, IOException] {
  override def decode[R <: Any, E >: IOException](resource: BinaryResource[R, E]): ZipFileResource[R, E] =
    new ZipFileResource[R, E] {

      override def fileName: Option[String] = None

      override def asBytes: ZStream[R, E, Byte] = resource.asBytes


      override def asZip: ZIO[R & Scope, E, ZipFileResource.Zip[R, E]] =
        dataStreamToUint8Array(resource.asBytes)
          .flatMap { data =>
            ZIO.fromPromiseJS {
              new JSZip().loadAsync(data)
            }.orDie
          }
          .map(ZipImpl(_))


      private final class ZipImpl(zip: JSZip) extends ZipFileResource.Zip[R, E]:
        override def getEntry(path: String): ZIO[R & Scope, E, Option[ZipStreamResource.Entry[R, E]]] =
          ZIO.succeed {
            Nullable(zip.file(path).asInstanceOf[JSZipObject | Null]).toOption.map(createEntry(path, _))
          }

        override def entries: ZStream[R, E, ZipStreamResource.Entry[R, E]] =
          ZStream.unwrap(
            for
              queue <- Queue.unbounded[Exit[Option[E], ZipStreamResource.Entry[R, E]]]
              runtime <- ZIO.runtime[R]
              _ <- (
                ZIO.succeed {
                  zip.forEach { (path, obj) =>
                    runtime.unsafe.fork(queue.offer(Exit.succeed(createEntry(path, obj))))
                  }
                }.foldCauseZIO(
                  failure = cause => queue.offer(Exit.failCause(cause.map(Some.apply))),
                  success = _ => queue.offer(Exit.fail(None)),
                )
              ).forkDaemon
            yield ZStream.fromQueue(queue).flattenExitOption
          )
      end ZipImpl

      private def createEntry(path2: String, entry: JSZipObject): ZipStreamResource.Entry[R, E] =
        new ZipStreamResource.Entry[R, E] {
          override val path: String = path2

          override def value: BinaryResource[R, E] =
            new BinaryResource[R, E] {
              override def fileName: Option[String] = None

              override def asBytes: ZStream[R, E, Byte] =
                ZStream.unwrap(
                  ZIO.fromPromiseJS(entry.async("uint8array"))
                    .orDie
                    .map(arr => ZStream.fromChunk(uint8ArrayToChunk(arr)))
                )
            }

        }

    }
  end decode
}
