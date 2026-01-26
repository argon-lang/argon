package dev.argon.backend.scalaApi

import zio.*

import dev.argon.io

object DirectoryResourceExtensions {
  extension [E] (dr: DirectoryResource[E])

    def toIOResource: io.DirectoryResource[E, io.BinaryResource] =
      new io.DirectoryResource[E, io.BinaryResource] {
        override def fileName: Option[String] = None

        override def contents: zio.stream.Stream[E, io.DirectoryEntry[E, io.BinaryResource]] =
          zio.stream.ZStream.unwrap(
            dr.contents().map { contents =>
              StreamUtil.toZStreamScoped(contents)
                .map { entry =>
                  io.DirectoryEntry(entry.dirs, entry.fileName, entry.resource.toIOResource)
                }
            }
          )
      }

  end extension
}
