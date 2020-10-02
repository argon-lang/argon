package dev.argon.platform

import cats._
import cats.implicits._
import java.io.{ByteArrayOutputStream, IOException, InputStream}
import java.nio.charset.CharsetDecoder
import java.nio.{ByteBuffer, CharBuffer}

import dev.argon.io._
import dev.argon.io.fileio.FileIO
import dev.argon.io.Path.PathExtensions
import dev.argon.platform.FileIOCommon
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

  override def getAbsolutePath(path: FilePath): IO[Throwable, FilePath] =
    IO.effect { toFilePath(JSPath.resolve(path.pathName)) }

  override def ensureParentDirectory(path: FilePath): IO[Throwable, Unit] =
    ZIO.foreach(path.parent) { dir =>
      IO.effectAsync[Throwable, Unit] { register =>
        NodeFileSystem.mkdir(dir.pathName, NodeFSMkdirOptions(recursive = true), error =>
          register(
            if(error != null)
              IO.fail(handleJSError(error))
            else
              IO.unit
          )
        )
      }
    }.unit


  override def readFile[E](errorHandler: Throwable => Cause[E])(path: FilePath): Stream[E, Byte] =
    readByteChunks(errorHandler)(path).map { arr =>
      val chunkBuilder = ChunkBuilder.make[Byte]()
      chunkBuilder.sizeHint(arr)
      arr.foreach { b =>
        chunkBuilder.addOne(b.toByte)
      }

      chunkBuilder.result()
    }
    .flattenChunks

  override def readAllText(path: FilePath): IO[Throwable, String] =
    IO.effectAsync { register =>
      NodeFileSystem.readFile(path.pathName, "utf-8", (error, data) =>
        register(
          if(error != null)
            IO.fail(handleJSError(error))
          else
            IO.succeed(data)
        )
      )
    }


  private def readByteChunks[E](errorHandler: Throwable => Cause[E])(path: FilePath): Stream[E, Uint8Array] =
    ZStream[Any, E, Uint8Array](
      ZManaged.make(
        IO.effectAsync[E, Integer] { register =>
          NodeFileSystem.open(path.pathName, "r", (error, fd) =>
            register(
              if(error != null)
                IO.halt(errorHandler(handleJSError(error)))
              else
                IO.succeed(fd)
            )
          )
        }.either
      )({
        case Left(_) => IO.unit
        case Right(fd) =>
          IO.effectAsync[Throwable, Unit] { register =>
            NodeFileSystem.close(fd, error =>
              register(
                if(error != null)
                  IO.fail(handleJSError(error))
                else
                  IO.succeed(())
              )
            )
          }.orDie
      })
        .map {
          case Left(e) => IO.fail(Some(e))
          case Right(fd) =>
            IO.effectAsync[Option[E], Chunk[Uint8Array]] { register =>
              val buffer = new Uint8Array(4096)
              NodeFileSystem.read(fd, buffer, 0, buffer.length, null, (err, bytesRead, _) =>
                register(
                  if(err != null)
                    IO.halt(errorHandler(handleJSError(err))).some
                  else if(bytesRead.toInt === 0)
                    IO.fail(None)
                  else
                    IO.succeed(Chunk(buffer.subarray(0, bytesRead)))
                )
              )
            }
        }
    )


  override def readText[E](errorHandler: Throwable => Cause[E])(path: FilePath): Stream[E, Char] =
    ZStream.fromEffect(IO.effectTotal { new JSStringDecoder("utf8") })
      .flatMap { decoder =>
        readByteChunks(errorHandler)(path)
          .mapM { arr => IO.effectTotal { decoder.write(arr) } } ++
          ZStream.fromEffect(IO.effectTotal { decoder.end() })
      }
      .flatMap(ZStream.fromIterable(_))


  override def writeToFile[R, E](errorHandler: Throwable => Cause[E])(path: FilePath)(data: ZStream[R, E, Byte]): ZIO[R, E, Unit] =
    IO.effectAsync[E, Integer] { register =>
      NodeFileSystem.open(path.pathName, "w", (error, fd) =>
        register(
          if(error != null)
            IO.halt(errorHandler(handleJSError(error)))
          else
            IO.succeed(fd)
        )
      )
    }
      .bracket(fd =>
        IO.effectAsync[Throwable, Unit] { register =>
          NodeFileSystem.close(fd, error =>
            register(
              if(error != null)
                IO.fail(handleJSError(error))
              else
                IO.succeed(())
            )
          )
        }.orDie
      ) { fd =>
        data.foreachChunk { chunk =>
          IO.effectAsync[E, Unit] { register =>
            val u8arr = new Uint8Array(chunk.size)
            for(i <- chunk.indices) {
              u8arr(i) = (chunk.byte(i) & 0xFF).toShort
            }

            NodeFileSystem.write(fd, u8arr, (err, _, _) =>
              register(
                if(err != null)
                  IO.halt(errorHandler(handleJSError(err)))
                else
                  IO.succeed(())
              )
            )
          }
        }
      }


  override def isDirectory(path: FilePath): IO[Throwable, Boolean] =
    IO.effectAsync { register =>
      NodeFileSystem.stat(path.pathName, (err, stat) =>
        register(
          if(err != null)
            IO.fail(handleJSError(err))
          else
            IO.succeed(stat.isDirectory())
        )
      )
    }

  override def listDirectory(path: FilePath): Stream[Throwable, FilePath] =
    Stream.fromEffect(
      IO.effectAsync[Throwable, Vector[FilePath]] { register =>
        NodeFileSystem.readdir(path.pathName, (err, files) =>
          register(
            if(err != null)
              IO.fail(handleJSError(err))
            else
              IO.succeed(files.map { fileName => toFilePath(JSPath.join(path.pathName, fileName)) }.toVector)
          )
        )
      }
    )
      .flatMap(ZStream.fromIterable(_))

  override def openZipFile[R, E](errorHandler: Throwable => Cause[E])(path: FilePath): Managed[E, ZipFileReader[R, E]] = {
    def handleNodeStreamZipError(err: Any): IO[E, Nothing] =
      IO.halt(errorHandler(handleJSError(err)))

    Managed.make(
      IO.effectAsync[E, NodeStreamZip] { register =>
        val zip = new NodeStreamZip(NodeStreamZip.Options(path.pathName))
        zip.on("ready", () => register(IO.succeed(zip)))
        zip.on("error", err => register(handleNodeStreamZipError(err)))
      }
    ) { zip =>
      IO.effectTotal { zip.close() }
    }
      .map { zip =>
        new ZipFileReader[R, E] {
          override def getEntryStream(name: String): ZIO[R, E, Option[ZStream[R, E, Byte]]] =
            IO.effectAsync[E, Option[NodeReadable]] { register =>
              zip.stream(name, (err, stream) => register(
                if(err == null)
                  IO.some(stream)
                else if(err == "Entry not found" || err == "Entry is not file")
                  IO.none
                else
                  handleNodeStreamZipError(err)
              ))
            }
              .map { _.map { stream =>
                WritableZStream { wzs =>
                  IO.effectAsync[Any, Unit] { register =>
                    stream.pipe(wzs)
                    stream.on("end", () => register(IO.succeed(())))
                  }
                }
                  .catchAll { err => Stream.halt(errorHandler(handleJSError(err))) }
              } }

        }
      }
  }


}
