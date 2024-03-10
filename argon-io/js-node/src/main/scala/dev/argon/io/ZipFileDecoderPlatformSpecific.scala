package dev.argon.io

import dev.argon.util.*
import zio.*
import zio.stream.*

import java.io.IOException
import scala.scalajs.js.typedarray.{Int8Array, Uint8Array}
import dev.argon.io.jstypes.jszip.JSZip
import dev.argon.io.jstypes.jszip.JSZip.JSZipObject

final class ZipFileDecoderPlatformSpecific extends BinaryResourceDecoder[ZipFileResource, IOException] {
  override def decode[E >: IOException](resource: BinaryResource[E]): ZipFileResource[E] =
    new ZipFileResource[E] {

      override def fileName: Option[String] = None

      override def asBytes: ZStream[Any, E, Byte] = resource.asBytes


      override def asZip: ZIO[Scope, E, ZipFileResource.Zip[E]] =
        dataStreamToUint8Array(resource.asBytes)
          .flatMap { data =>
            ZIO.fromPromiseJS {
              new JSZip().loadAsync(data)
            }.orDie
          }
          .map(ZipImpl(_))


      private final class ZipImpl(zip: JSZip) extends ZipFileResource.Zip[E]:
        override def getEntry(path: String): ZIO[Scope, E, Option[ZipStreamResource.Entry[E]]] =
          ZIO.succeed {
            zip.file(path).asInstanceOf[JSZipObject | Null].toOption.map(createEntry(path, _))
          }

        override def entries: ZStream[Any, E, ZipStreamResource.Entry[E]] =
          ZStream.unwrap(
            for
              queue <- Queue.unbounded[Exit[Option[E], ZipStreamResource.Entry[E]]]
              runtime <- ZIO.runtime[Any]
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

      private def createEntry(path2: String, entry: JSZipObject): ZipStreamResource.Entry[E] =
        new ZipStreamResource.Entry[E] {
          override val path: String = path2

          override def value: BinaryResource[E] =
            new BinaryResource[E] {
              override def fileName: Option[String] = None

              override def asBytes: ZStream[Any, E, Byte] =
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
