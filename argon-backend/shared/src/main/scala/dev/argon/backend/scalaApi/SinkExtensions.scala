package dev.argon.backend.scalaApi

import dev.argon.io
import zio.stream.ZSink
import ScopedResourceExtensions.*
import zio.*

object SinkExtensions {
  
  extension [E](sink: BinaryResourceSink[E])
    def toZSink: ZSink[Any, E, Byte, Nothing, Unit] =
      ZSink.unwrap(sink.sink().map(scopedBinaryWriterToSink))
      
    def write[E1 >: E](resource: io.BinaryResource[E1]): IO[E1, Unit] =
      resource.asBytes.run(toZSink)
  end extension
  
  private def scopedBinaryWriterToSink[E](scopedWriter: ScopedResource[E, BinaryWriter[E]]): ZSink[Any, E, Byte, Nothing, Unit] =
    ZSink.fromPush(
      for
        scopeEnv <- ZIO.environment[Scope]
        getWriter <- scopedWriter.toScopeIO.memoize
      yield (chunkOpt: Option[Chunk[Byte]]) => chunkOpt match {
        case Some(chunk) =>
          getWriter
            .flatMap(_.write(chunk))
            .mapError(e => (Left(e), Chunk.empty))
            .provideEnvironment(scopeEnv)
        case None => ZIO.fail((Right(()), Chunk.empty))
      }
    )
    
  
  extension [E](sink: DirectoryResourceSink[E])
    def writeAll[E1 >: E](resource: io.DirectoryResource[E1, io.BinaryResource]): IO[E1, Unit] =
      ZIO.scoped(
        sink.sink()
          .flatMap(_.toScopeIO)
          .flatMap { dirWriter =>
            resource.contents.foreach { entry =>
              dirWriter.write(entry.dirs, entry.fileName).flatMap { scopedWriter =>
                entry.resource.asBytes.run(scopedBinaryWriterToSink(scopedWriter))
              }
            }
          }
      )

}
