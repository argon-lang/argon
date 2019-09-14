package dev.argon.io

import cats._
import cats.implicits._
import java.io.{ByteArrayOutputStream, IOException}
import java.nio.charset.CharsetDecoder
import java.nio.{ByteBuffer, CharBuffer}

import dev.argon.stream.builder.{Source, SourceIO, ZStreamSource}
import scalapb.{GeneratedMessage, GeneratedMessageCompanion, Message}
import zio._
import zio.stream._
import zio.console.Console
import zio.stream.ZSink.Step
import zio.system.System

import scala.scalajs.js
import scala.scalajs.js.typedarray.Uint8Array
import scala.scalajs.js.JSConverters._
import scala.scalajs.js.|

@SuppressWarnings(Array("org.wartremover.warts.Equals", "org.wartremover.warts.ToString", "org.wartremover.warts.Null"))
class NodeIOEnvironment(otherEnv: Console with System) extends FileIO with Console with System {

  override val fileIO: FileIO.Service = new FileIO.Service {
    override def getAbsolutePath(path: Path): IO[IOException, Path] =
      IO.effect { new Path(JSPath.resolve(path.pathName)) }
        .refineOrDie { case e: IOException => e }

    override def readAllText(path: Path): IO[IOException, String] =
      IO.effectAsync { register =>
        JSFileSystem.readFile(path.toString, "utf-8", (error, data) =>
          register(
            if(error != null)
              IO.fail(JSIOException(error))
            else
              IO.succeed(data)
          )
        )
      }


    private def readByteChunks[E](errorHandler: IOException => E)(path: Path): Stream[E, Uint8Array] =
      ZStream[Any, E, Uint8Array](
        ZManaged.make(
          IO.effectAsync[E, Integer] { register =>
            JSFileSystem.open(path.toString, "r", (error, fd) =>
              register(
                if(error != null)
                  IO.fail(errorHandler(JSIOException(error)))
                else
                  IO.succeed(fd)
              )
            )
          }
        )(
          fd => IO.effectAsync[IOException, Unit] { register =>
            JSFileSystem.close(fd, error =>
              register(
                if(error != null)
                  IO.fail(JSIOException(error))
                else
                  IO.succeed(())
              )
            )
          }.orDie
        ).map { fd =>
          IO.effectAsync[Option[E], Uint8Array] { register =>
            val buffer = new Uint8Array(4096)
            JSFileSystem.read(fd, buffer, 0, buffer.length, null, (err, bytesRead, _) =>
              register(
                if(err != null)
                  IO.fail(Some(errorHandler(JSIOException(err))))
                else if(bytesRead.toInt === 0)
                  IO.fail(None)
                else
                  IO.succeed(buffer.subarray(0, bytesRead))
              )
            )
          }
        }
      )


    override def readText[E](errorHandler: IOException => E)(path: Path): Stream[E, Char] =
      ZStream.fromEffect(IO.effectTotal { new JSStringDecoder("utf8") })
        .flatMap { decoder =>
          readByteChunks(errorHandler)(path)
            .mapM { arr => IO.effectTotal { decoder.write(arr) } } ++
              ZStream.fromEffect(IO.effectTotal { decoder.end() })
        }
        .flatMap(ZStream.fromIterable(_))

    override def writeToFile[R, E, X](errorHandler: IOException => E)(path: Path)(data: Source[ZIO[R, E, *], Chunk[Byte], X]): ZIO[R, E, X] =
      IO.effectAsync[E, Integer] { register =>
        JSFileSystem.open(path.toString, "r", (error, fd) =>
          register(
            if(error != null)
              IO.fail(errorHandler(JSIOException(error)))
            else
              IO.succeed(fd)
          )
        )
      }
        .bracket(fd =>
          IO.effectAsync[IOException, Unit] { register =>
            JSFileSystem.close(fd, error =>
              register(
                if(error != null)
                  IO.fail(JSIOException(error))
                else
                  IO.succeed(())
              )
            )
          }.orDie
        ) { fd =>
          data.foreach { chunk =>
            IO.effectAsync[E, Unit] { register =>
              JSFileSystem.write(fd, new Uint8Array(chunk.toArray.toJSArray), (err, _, _) =>
                register(
                  if(err != null)
                    IO.fail(errorHandler(JSIOException(err)))
                  else
                    IO.succeed(())
                )
              )
            }
          }
        }


    override def isDirectory(path: Path): IO[IOException, Boolean] =
      IO.effectAsync { register =>
        JSFileSystem.stat(path.toString, (err, stat) =>
          register(
            if(err != null)
              IO.fail(JSIOException(err))
            else
              IO.succeed(stat.isDirectory())
          )
        )
      }

    override def listDirectory(path: Path): Stream[IOException, Path] =
      Stream.fromEffect(
        IO.effectAsync[IOException, Vector[Path]] { register =>
          JSFileSystem.readdir(path.toString, (err, files) =>
            register(
              if(err != null)
                IO.fail(JSIOException(err))
              else
                IO.succeed(files.map(new Path(_)).toVector)
            )
          )
        }
      )
        .flatMap(ZStream.fromIterable(_))

    private def dataStreamToArray[R, E](dataStream: Source[ZIO[R, E, *], Chunk[Byte], Unit]): ZIO[R, E, Uint8Array] =
      IO.effectTotal { new ByteArrayOutputStream() }
        .flatMap { outputStream =>
          dataStream.foreach { chunk =>
            IO.effectTotal { outputStream.write(chunk.toArray) }
          }
            .flatMap { _ =>
              IO.effectTotal { new Uint8Array(outputStream.toByteArray.toJSArray) }
            }
        }

    private def promiseToIO[E, A](errorHandler: IOException => E)(promise: => js.Promise[A]): IO[E, A] =
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
                  dataStreamToArray(entry.dataStream).flatMap { buffer =>
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

    override def openZipFile[R, E](errorHandler: IOException => E)(path: Path): Managed[E, ZipFileReader[ZIO[R, E, *]]] =
      ZManaged.fromEffect(
        dataStreamToArray[Any, E](ZStreamSource(
          readByteChunks[E](errorHandler)(path)
            .map { data => Chunk.fromArray(data.toArray.map { _.toByte }) }
        ))
          .flatMap { data =>
            IO.effect { new JSZip(data) }
              .refineOrDie {
                case js.JavaScriptException(e: js.Error) => errorHandler(JSIOException(e))
              }
          }
          .map { zip =>
            new ZipFileReader[ZIO[R, E, *]] {
              override def getEntryStream(name: String): Source[ZIO[R, E, *], Chunk[Byte], Unit] =
                ZStreamSource(
                  ZStream.flatten(
                    ZStream.fromEffect(
                      promiseToIO(errorHandler)(zip.file(name).async("uint8array"))
                        .map { data => ZStream(Chunk.fromArray(data.toArray.map { _.toByte })) }
                    )
                  )
                )

            }
          }
      )

    override def deserializeProtocolBuffer[R, E, A <: GeneratedMessage with Message[A]](errorHandler: IOException => E)(companion: GeneratedMessageCompanion[A])(data: Source[ZIO[R, E, *], Chunk[Byte], Unit]): ZIO[R, E, A] =
      SourceIO.fromSource(data).toZStream.foldLeft[Chunk[Byte], Chunk[Byte]](Chunk.empty) { _ ++ _ }
        .flatMap { data =>
          IO.effect {
            companion.parseFrom(data.toArray)
          }
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
  override val console: Console.Service[Any] = otherEnv.console
  override val system: System.Service[Any] = otherEnv.system
}
