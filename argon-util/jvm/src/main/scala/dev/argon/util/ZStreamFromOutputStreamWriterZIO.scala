package dev.argon.util

import java.io.OutputStream
import zio.*
import zio.stream.ZStream
import java.io.PipedOutputStream
import java.io.PipedInputStream

object ZStreamFromOutputStreamWriterZIO {

  def apply[R, E](write: OutputStream => ZIO[R, E, Unit], chunkSize: Int = ZStream.DefaultChunkSize)
    : ZStream[R, E, Byte] =
    ZStream.fromZIO(ZIO.succeed { new PipedOutputStream() }).flatMap { out =>
      ZStream.scoped(ZIO.fromAutoCloseable(ZIO.succeed { new PipedInputStream(out) })).flatMap { in =>
        ZStream.fromZIO(Promise.make[None.type, Nothing]).flatMap { done =>
          (ZStream.fromInputStream(in, chunkSize).refineOrDie(PartialFunction.empty) ++ ZStream.fromZIOOption(
            done.await
          )).drainFork(
            ZStream.fromZIO(
              write(out).ensuring { ZIO.attemptBlockingInterrupt { out.close() }.orDie }
            ) ++ ZStream.fromZIO(done.fail(None))
          )
        }
      }
    }
  end apply

}
