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
import zio.system.System

import scala.scalajs.js
import scala.scalajs.js.typedarray.Uint8Array
import scala.scalajs.js.JSConverters._
import scala.scalajs.js.|
import dev.argon.util.NodeProcess

@SuppressWarnings(Array("org.wartremover.warts.Equals", "org.wartremover.warts.Null"))
class NodeIOService extends FileIOServiceCommon {

  override def getEnv(name: String): UIO[Option[String]] =
    IO.effectTotal { NodeProcess.env(name).toOption }

  override def readAllText(path: Path): IO[IOException, String] =
    IO.effectAsync { register =>
      NodeFileSystem.readFile(path.toString, "utf-8", (error, data) =>
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
          NodeFileSystem.open(path.toString, "r", (error, fd) =>
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
      NodeFileSystem.open(path.toString, "w", (error, fd) =>
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


  override def isDirectory(path: Path): IO[IOException, Boolean] =
    IO.effectAsync { register =>
      NodeFileSystem.stat(path.toString, (err, stat) =>
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
        NodeFileSystem.readdir(path.toString, (err, files) =>
          register(
            if(err != null)
              IO.fail(JSIOException(err))
            else
              IO.succeed(files.map { fileName => new Path(JSPath.join(path.pathName, fileName)) }.toVector)
          )
        )
      }
    )
      .flatMap(ZStream.fromIterable(_))


  override def openZipFile[R, E](errorHandler: IOException => E)(path: Path): Managed[E, ZipFileReader[ZIO[R, E, *]]] =
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
        new ZipFileReader[ZIO[R, E, *]] {
          override def getEntryStream(name: String): Source[ZIO[R, E, *], Chunk[Byte], Unit] =
            ZStreamSource(
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
