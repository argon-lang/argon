package dev.argon.platform

import cats._
import cats.implicits._
import java.io.{ByteArrayOutputStream, IOException}
import java.nio.charset.CharsetDecoder
import java.nio.{ByteBuffer, CharBuffer}

import dev.argon.io._
import dev.argon.io.fileio.FileIO
import dev.argon.platform.FileIOCommon
import dev.argon.stream.builder.{Source, ZStreamSource}
import scalapb.{GeneratedMessage, GeneratedMessageCompanion, Message}
import zio._
import zio.stream._
import zio.console.Console
import zio.system.System

import scala.scalajs.js
import scala.scalajs.js.typedarray.Uint8Array
import scala.scalajs.js.JSConverters._
import scala.scalajs.js.|

@SuppressWarnings(Array("org.wartremover.warts.Equals", "org.wartremover.warts.Null", "dev.argon.warts.ZioEffect"))
private[platform] class NodeIOService extends FileIO.Service[FilePath] with FileIOCommon {

  override def getAbsolutePath(path: FilePath): IO[IOException, FilePath] =
    IO.effect { toFilePath(JSPath.resolve(path.pathName)) }
      .refineOrDie { case e: IOException => e }

  override def readAllText(path: FilePath): IO[IOException, String] =
    IO.effectAsync { register =>
      NodeFileSystem.readFile(path.pathName, "utf-8", (error, data) =>
        register(
          if(error != null)
            IO.fail(JSIOException(error))
          else
            IO.succeed(data)
        )
      )
    }


  private def readByteChunks[E](errorHandler: IOException => E)(path: FilePath): Stream[E, Uint8Array] =
    ZStream[Any, E, Uint8Array](
      ZManaged.make(
        IO.effectAsync[E, Integer] { register =>
          NodeFileSystem.open(path.pathName, "r", (error, fd) =>
            register(
              if(error != null)
                IO.fail(errorHandler(JSIOException(error)))
              else
                IO.succeed(fd)
            )
          )
        }.either
      )({
        case Left(_) => IO.unit
        case Right(fd) =>
          IO.effectAsync[IOException, Unit] { register =>
            NodeFileSystem.close(fd, error =>
              register(
                if(error != null)
                  IO.fail(JSIOException(error))
                else
                  IO.succeed(())
              )
            )
          }.orDie
      })
        .map {
          case Left(e) => IO.fail(Some(e))
          case Right(fd) =>
            IO.effectAsync[Option[E], Uint8Array] { register =>
              val buffer = new Uint8Array(4096)
              NodeFileSystem.read(fd, buffer, 0, buffer.length, null, (err, bytesRead, _) =>
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


  override def readText[E](errorHandler: IOException => E)(path: FilePath): Stream[E, Char] =
    ZStream.fromEffect(IO.effectTotal { new JSStringDecoder("utf8") })
      .flatMap { decoder =>
        readByteChunks(errorHandler)(path)
          .mapM { arr => IO.effectTotal { decoder.write(arr) } } ++
          ZStream.fromEffect(IO.effectTotal { decoder.end() })
      }
      .flatMap(ZStream.fromIterable(_))

  override def writeToFile[R, E, X](errorHandler: IOException => E)(path: FilePath)(data: Source[R, E, Chunk[Byte], X]): ZIO[R, E, X] =
    IO.effectAsync[E, Integer] { register =>
      NodeFileSystem.open(path.pathName, "w", (error, fd) =>
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
          NodeFileSystem.close(fd, error =>
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
            NodeFileSystem.write(fd, new Uint8Array(chunk.toArray.toJSArray), (err, _, _) =>
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


  override def isDirectory(path: FilePath): IO[IOException, Boolean] =
    IO.effectAsync { register =>
      NodeFileSystem.stat(path.pathName, (err, stat) =>
        register(
          if(err != null)
            IO.fail(JSIOException(err))
          else
            IO.succeed(stat.isDirectory())
        )
      )
    }

  override def listDirectory(path: FilePath): Stream[IOException, FilePath] =
    Stream.fromEffect(
      IO.effectAsync[IOException, Vector[FilePath]] { register =>
        NodeFileSystem.readdir(path.pathName, (err, files) =>
          register(
            if(err != null)
              IO.fail(JSIOException(err))
            else
              IO.succeed(files.map { fileName => toFilePath(JSPath.join(path.pathName, fileName)) }.toVector)
          )
        )
      }
    )
      .flatMap(ZStream.fromIterable(_))


  override def openZipFile[R, E](errorHandler: IOException => E)(path: FilePath): Managed[E, ZipFileReader[R, E]] =
    Managed.make(
      IO.effectAsync[E, NodeStreamZip] { register =>
        val zip = new NodeStreamZip(NodeStreamZip.Options(path.pathName))
        zip.on("ready", () => register(IO.succeed(zip)))
        zip.on("error", err => register(IO.fail(errorHandler(JSIOException(err)))))
      }
    ) { zip =>
      IO.effectTotal { zip.close() }
    }
      .map { zip =>
        new ZipFileReader[R, E] {
          override def getEntryStream(name: String): Source[R, E, Chunk[Byte], Unit] =
            new ZStreamSource(
              ZStream.fromEffect(
                IO.effectAsync[E, NodeReadable] { register =>
                  zip.stream(name, (err, stream) => register(
                    if(err == null)
                      IO.succeed(stream)
                    else
                      IO.fail(errorHandler(JSIOException(err)))
                  ))
                }
              )
                .flatMap { stream =>
                  WritableZStream { wzs =>
                    IO.effectAsync { register =>
                      stream.pipe(wzs)
                      stream.on("end", () => register(IO.succeed(())))
                    }
                  }
                    .mapError { err => errorHandler(JSIOException(err)) }
                }
            )
        }
      }


}
