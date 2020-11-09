package dev.argon.platform

import java.io.IOException

import com.google.protobuf.CodedInputStream
import dev.argon.io.{StreamableMessage, ZipEntryInfo}
import dev.argon.io.fileio.FileIOLite
import scalapb.{GeneratedMessage, GeneratedMessageCompanion}
import zio._
import zio.stream.ZStream

import scala.collection.mutable

@SuppressWarnings(Array("dev.argon.warts.ZioEffect"))
private[platform] trait FileIOLiteServiceCommon extends FileIOLite.Service with FileIOCommon {


  override def zipEntries[R, E <: Throwable](errorHandler: Throwable => Cause[E])(entries: ZStream[R, E, ZipEntryInfo[R, E]]): ZStream[R, E, Byte] =
    ZStream.unwrap(
      IO.effectTotal { new JSZip() }
        .flatMap { zip =>
          entries.foreach { entry =>
            dataStreamToUint8Array(entry.dataStream).flatMap { buffer =>
              IO.effect { zip.file(entry.path, buffer) }
                .orDie
                .unit
            }
          }
            .flatMap { _ =>
              promiseToIO(errorHandler)(zip.generateAsync(JSZip.JSZipGeneratorOptions("uint8array")))
                .map { data =>
                  val arr = new Array[Byte](data.length)
                  for(i <- arr.indices) {
                    arr(i) = data(i).toByte
                  }

                  ZStream.fromChunk(Chunk.fromArray(arr))
                }
            }
        }
    )

  override def deserializeProtocolBuffer[R, E <: Throwable, A <: GeneratedMessage](errorHandler: Throwable => Cause[E])(companion: GeneratedMessageCompanion[A])(data: ZStream[R, E, Byte]): ZIO[R, E, A] =
    dataStreamToArray(data)
      .flatMap { data =>
        IO.effect { companion.parseFrom(data) }
          .catchAll { ex => IO.halt(errorHandler(ex)) }
      }

  override def serializeProtocolBuffer[E <: Throwable](errorHandler: Throwable => Cause[E])(message: GeneratedMessage): stream.Stream[E, Byte] =
    ZStream.unwrap(
      IO.effect {
        message.toByteArray
      }
        .catchAll { ex => IO.halt(errorHandler(ex)) }
        .map { data => ZStream.fromChunk(Chunk.fromArray(data)) }
    )

  @SuppressWarnings(Array("org.wartremover.warts.Equals", "org.wartremover.warts.Null", "org.wartremover.warts.NonUnitStatements"))
  override def deserializeProtocolBufferStream[R, E <: Throwable, A >: Null <: AnyRef](errorHandler: Throwable => Cause[E])(companion: StreamableMessage[A])(data: ZStream[R, E, Byte]): ZStream[R, E, A] = {
    ZStream.unwrap(
      dataStreamToArray(data)
        .flatMap { data =>
          IO.effect {
            val input = CodedInputStream.newInstance(data)
            val buffer = new mutable.ArrayBuffer[A]()

            def iter(): Unit = {
              val elem = companion.readElement(input)
              if(elem eq null) {
                ()
              }
              else {
                buffer += elem
                iter()
              }
            }

            iter()

            ZStream.fromIterable(buffer.toSeq)
          }
        }
        .catchAll { ex => IO.halt(errorHandler(ex)) }
    )
  }
}
