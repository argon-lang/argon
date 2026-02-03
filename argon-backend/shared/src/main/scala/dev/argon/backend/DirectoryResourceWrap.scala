package dev.argon.backend

import dev.argon.backend.scalaApi.{ScopedResource, StreamUtil}
import dev.argon.io.{BinaryResource, DirectoryEntry, DirectoryResource}
import dev.argon.util.async.ErrorWrapper
import zio.*
import zio.stream.*

import java.io.IOException
import scala.annotation.unused

object DirectoryResourceWrap {

  def wrap[E >: IOException](res: DirectoryResource[E, BinaryResource])(using ew: ErrorWrapper[E], @unused rt: Runtime[Any]): scalaApi.DirectoryResource[ew.EX] =
    new scalaApi.DirectoryResource[ew.EX] {
      override def contents(): UIO[ScopedResource[ew.EX, scalaApi.Stream[ew.EX, scalaApi.DirectoryEntry[ew.EX]]]] =
        StreamUtil.fromZStreamScoped(
          ErrorWrapper.wrapStream(
            res.contents
              .map { entry =>
                scalaApi.DirectoryEntry[ew.EX](entry.dirs, entry.fileName, BinaryResourceWrap.wrap(entry.resource))
              }
          )
        )
    }

  def unwrap[E](using ew: ErrorWrapper[E])(res: scalaApi.DirectoryResource[ew.EX]): DirectoryResource[E, BinaryResource] =
    new DirectoryResource[E, BinaryResource] {
      override def fileName: Option[String] = None

      override def contents: Stream[E, DirectoryEntry[E, BinaryResource]] =
        ZStream.unwrap(
          res.contents().map { contents =>
            ErrorWrapper.unwrapStream(StreamUtil.toZStreamScoped(contents))
              .map { entry =>
                DirectoryEntry(entry.dirs, entry.fileName, BinaryResourceWrap.unwrap(entry.resource))
              }
          }
        )
    }

}
