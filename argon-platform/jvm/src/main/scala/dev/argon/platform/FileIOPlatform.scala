package dev.argon.platform

import java.io.IOException
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

import zio._
import zio.blocking.Blocking
import zio.stream._
import dev.argon.io.fileio.FileIO


@SuppressWarnings(Array("dev.argon.warts.ZioEffect"))
object FileIOPlatform {

  val live: ZLayer[Blocking, Nothing, FileIO] = ZLayer.fromFunction { env =>
    val blocking = env.get[Blocking.Service]

    new FileIO.Service {

      override def readFile(path: String): Stream[Throwable, Byte] =
        ZStream.fromFile(Path.of(path))
          .provide(env)


      override def readAllText(path: String): IO[Throwable, String] =
        blocking.effectBlockingInterrupt { Files.readString(Path.of(path), StandardCharsets.UTF_8) }
          .refineOrDie { case e: IOException => e }

      override def readText(path: String): Stream[Throwable, Char] =
        ZStream.fromFile(Path.of(path))
          .transduce(ZTransducer.utf8Decode)
          .flatMap { s => ZStream.fromChunk(Chunk.fromArray(s.toCharArray)) }
          .provide(env)


      override def writeToFile[R](path: String)(data: ZStream[R, Throwable, Byte]): ZIO[R, Throwable, Unit] =
        data.run(ZSink.fromFile(Path.of(path)).provide(env)).unit

      override def isDirectory(path: String): IO[Throwable, Boolean] =
        blocking.effectBlockingInterrupt { Files.isDirectory(Path.of(path)) }

      @SuppressWarnings(Array("scalafix:Disable.toString"))
      override def listDirectory(path: String): Stream[Throwable, String] =
        ZStream[Any, Throwable, String](
          ZManaged.fromAutoCloseable(blocking.effectBlockingInterrupt { Files.list(Path.of(path)) })
            .foldCauseM(
              failure = cause => Managed.succeed(Exit.Failure(cause)),
              success = stream => Managed.fromEffect(blocking.effectBlockingInterrupt { stream.iterator() }.run)
            )
            .map { iteratorEither =>
              IO.done(iteratorEither).flatMap { iterator =>
                blocking.effectBlockingInterrupt {
                  if(iterator.hasNext)
                    IO.succeed(Chunk(iterator.next().toString))
                  else
                    IO.fail(None)
                }
              }.asSomeError.flatten
            }
        )


    }
  }

}
