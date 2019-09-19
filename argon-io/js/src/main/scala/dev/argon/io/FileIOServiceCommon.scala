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

trait FileIOServiceCommon extends FileIO.Service {
  override def getAbsolutePath(path: Path): IO[IOException, Path] =
    IO.effect { new Path(JSPath.resolve(path.pathName)) }
      .refineOrDie { case e: IOException => e }


  protected def dataStreamToArray[R, E](dataStream: Source[ZIO[R, E, *], Chunk[Byte], Unit]): ZIO[R, E, Array[Byte]] =
    IO.effectTotal { new ByteArrayOutputStream() }
      .flatMap { outputStream =>
        dataStream.foreach { chunk =>
          IO.effectTotal { outputStream.write(chunk.toArray) }
        }
          .flatMap { _ =>
            IO.effectTotal { outputStream.toByteArray }
          }
      }
  protected def dataStreamToUint8Array[R, E](dataStream: Source[ZIO[R, E, *], Chunk[Byte], Unit]): ZIO[R, E, Uint8Array] =
    dataStreamToArray(dataStream).map { arr => new Uint8Array(arr.toJSArray) }

  protected def promiseToIO[E, A](errorHandler: IOException => E)(promise: => js.Promise[A]): IO[E, A] =
    IO.effectAsync { register =>
      val _ = promise
        .`then`[Unit](
          onFulfilled = data => register(IO.succeed(data)),
          onRejected = {
            case e: js.Error => register(IO.fail(errorHandler(JSIOException(e))))
            case _ => register(IO.fail(errorHandler(new IOException("An unknown error occurred"))))
          } : js.Function1[Any, Unit | js.Thenable[Unit]]
        )
    }


  override def zipEntries[R, E](errorHandler: IOException => E)(entries: Source[ZIO[R, E, *], ZipEntryInfo[ZIO[R, E, *]], Unit]): Source[ZIO[R, E, *], Chunk[Byte], Unit] =
    ZStreamSource(
      ZStream.flatten(
        ZStream.fromEffect(
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
                    .map { data => ZStream(Chunk.fromArray(data.toArray.map { _.toByte })) }
                }
            }
        )
      )
    )

  override def deserializeProtocolBuffer[R, E, A <: GeneratedMessage with Message[A]](errorHandler: IOException => E)(companion: GeneratedMessageCompanion[A])(data: Source[ZIO[R, E, *], Chunk[Byte], Unit]): ZIO[R, E, A] =
    dataStreamToArray(SourceIO.fromSource(data))
      .flatMap { data =>
        IO.effect { companion.parseFrom(data) }
          .refineOrDie {
            case ex: IOException => errorHandler(ex)
          }
      }

  override def serializeProtocolBuffer[R, E](errorHandler: IOException => E)(message: GeneratedMessage): Source[ZIO[R, E, *], Chunk[Byte], Unit] =
    ZStreamSource(
      ZStream.fromEffect(
        IO.effect {
          message.toByteArray
        }
          .refineOrDie {
            case ex: IOException => errorHandler(ex)
          }
          .map { data => Chunk.fromArray(data) }
      )
    )

}
