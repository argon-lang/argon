package dev.argon.platform

import cats._
import cats.implicits._

import dev.argon.io._
import dev.argon.io.fileio.FileIO
import zio._
import zio.stream._

import scala.scalajs.js.typedarray.Uint8Array
import scala.scalajs.js.JSConverters._
import scala.scalajs.js.|

import dev.argon.util.JSErrorHandler._

@SuppressWarnings(Array("org.wartremover.warts.Equals", "org.wartremover.warts.Null", "dev.argon.warts.ZioEffect"))
private[platform] class NodeIOService extends FileIO.Service {

  override def readFile(path: String): Stream[Throwable, Byte] =
    readByteChunks(path).map { arr =>
      val chunkBuilder = ChunkBuilder.make[Byte]()
      chunkBuilder.sizeHint(arr)
      arr.foreach { b =>
        chunkBuilder.addOne(b.toByte)
      }

      chunkBuilder.result()
    }
    .flattenChunks

  override def readAllText(path: String): IO[Throwable, String] =
    IO.effectAsync { register =>
      NodeFileSystem.readFile(path, "utf-8", (error, data) =>
        register(
          if(error != null)
            IO.fail(handleJSError(error))
          else
            IO.succeed(data)
        )
      )
    }


  private def readByteChunks(path: String): Stream[Throwable, Uint8Array] =
    ZStream[Any, Throwable, Uint8Array](
      ZManaged.make(
        IO.effectAsync[Throwable, Integer] { register =>
          NodeFileSystem.open(path, "r", (error, fd) =>
            register(
              if(error != null)
                IO.fail(handleJSError(error))
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
            IO.effectAsync[Option[Throwable], Chunk[Uint8Array]] { register =>
              val buffer = new Uint8Array(4096)
              NodeFileSystem.read(fd, buffer, 0, buffer.length, null, (err, bytesRead, _) =>
                register(
                  if(err != null)
                    IO.fail(Some(handleJSError(err)))
                  else if(bytesRead.toInt === 0)
                    IO.fail(None)
                  else
                    IO.succeed(Chunk(buffer.subarray(0, bytesRead)))
                )
              )
            }
        }
    )


  override def readText(path: String): Stream[Throwable, Char] =
    ZStream.fromEffect(IO.effectTotal { new JSStringDecoder("utf8") })
      .flatMap { decoder =>
        readByteChunks(path)
          .mapM { arr => IO.effectTotal { decoder.write(arr) } } ++
          ZStream.fromEffect(IO.effectTotal { decoder.end() })
      }
      .flatMap(ZStream.fromIterable(_))


  override def writeToFile[R](path: String)(data: ZStream[R, Throwable, Byte]): ZIO[R, Throwable, Unit] =
    IO.effectAsync[Throwable, Integer] { register =>
      NodeFileSystem.open(path, "w", (error, fd) =>
        register(
          if(error != null)
            IO.fail(handleJSError(error))
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
          IO.effectAsync[Throwable, Unit] { register =>
            val u8arr = new Uint8Array(chunk.size)
            for(i <- chunk.indices) {
              u8arr(i) = (chunk.byte(i) & 0xFF).toShort
            }

            NodeFileSystem.write(fd, u8arr, (err, _, _) =>
              register(
                if(err != null)
                  IO.fail(handleJSError(err))
                else
                  IO.succeed(())
              )
            )
          }
        }
      }


  override def isDirectory(path: String): IO[Throwable, Boolean] =
    IO.effectAsync { register =>
      NodeFileSystem.stat(path, (err, stat) =>
        register(
          if(err != null)
            IO.fail(handleJSError(err))
          else
            IO.succeed(stat.isDirectory())
        )
      )
    }

  override def listDirectory(path: String): Stream[Throwable, String] =
    Stream.fromEffect(
      IO.effectAsync[Throwable, Vector[String]] { register =>
        NodeFileSystem.readdir(path, (err, files) =>
          register(
            if(err != null)
              IO.fail(handleJSError(err))
            else
              IO.succeed(files.map { fileName => FileNameUtil.combine(path, fileName) }.toVector)
          )
        )
      }
    )
      .flatMap(ZStream.fromIterable(_))


}
