package dev.argon.backend

import dev.argon.backend.scalaApi.ScopedResource
import dev.argon.io.{BinaryResource, DirectoryEntry, DirectoryResource}
import dev.argon.util.async.ErrorWrapper
import zio.*
import zio.stream.*

import java.io.IOException

private[backend] object DirectoryResourceWrap {

  def wrap[E >: IOException](res: DirectoryResource[E, BinaryResource])(using ew: ErrorWrapper[E], rt: Runtime[Any]): scalaApi.DirectoryResource[ew.EX] =
    new scalaApi.DirectoryResource[ew.EX] {
      override def contents(): IO[ew.EX, ScopedResource[scalaApi.Stream[ew.EX, scalaApi.DirectoryEntry[ew.EX]]]] =
        StreamWrap.wrapStream(
          res.contents
            .map { entry =>
              scalaApi.DirectoryEntry[ew.EX](entry.dirs, entry.fileName, BinaryResourceWrap.wrap(entry.resource))
            }
        )
    }

  def unwrap[E](using ew: ErrorWrapper[E])(res: scalaApi.DirectoryResource[ew.EX]): DirectoryResource[E, BinaryResource] =
    new DirectoryResource[E, BinaryResource] {
      override def fileName: Option[String] = None

      override def contents: Stream[E, DirectoryEntry[E, BinaryResource]] =
        StreamWrap.unwrapStream(res.contents())
          .map { entry =>
            DirectoryEntry(entry.dirs, entry.fileName, BinaryResourceWrap.unwrap(entry.resource))
          }
    }

}
