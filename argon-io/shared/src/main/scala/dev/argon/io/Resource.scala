package dev.argon.io

import zio.*
import zio.stream.*

import java.io.IOException
import java.nio.charset.CharacterCodingException
import dev.argon.util.async.ErrorWrapper

abstract class Resource[+E] {
  def fileName: Option[String]
}

object Resource:
  trait WithoutFileName:
    def fileName: Option[String] = None
  end WithoutFileName
end Resource


abstract class BinaryResource[+E] extends Resource[E] with BinaryResourcePlatformSpecific[E] {
  def asBytes: Stream[E, Byte]

  def byteSize: Option[ZIO[Any, E, BigInt]] = None
}

object BinaryResource:
  extension [E](res: BinaryResource[E]) {
    def decode[Res[+E2] <: Resource[E2]]: BinaryResourceDecodePartiallyApplied[E, Res] =
      BinaryResourceDecodePartiallyApplied(res)
  }
  
  final case class BinaryResourceDecodePartiallyApplied[E, Res[+E2] <: Resource[E2]](res: BinaryResource[E]) extends AnyVal {
    def withError[E2 >: E](using decoder: BinaryResourceDecoder[Res, E2]): Res[E2] = decoder.decode(res)
  }
  
  object BinaryResourceDecodePartiallyApplied {
    given [E, E2 >: E, Res[+E3] <: Resource[E3]] => (decoder: BinaryResourceDecoder[Res, E2]) => Conversion[BinaryResourceDecodePartiallyApplied[E, Res], Res[E2]]:
      override def apply(x: BinaryResourceDecodePartiallyApplied[E, Res]): Res[E2] = decoder.decode(x.res)
    end given
  }

  given [E] => BinaryResourceDecoder[BinaryResource, E]:
    def decode(resource: BinaryResource[E]): BinaryResource[E] =
      resource
  end given
end BinaryResource

sealed trait FileSystemResource[+E, +FileResource[+E2] <: Resource[E2]]

object FileSystemResource {
  final case class Of[+E, +FileResource[+E2] <: Resource[E2]](fileResource: FileResource[E]) extends FileSystemResource[E, FileResource]
}

abstract class DirectoryResource[+E, +FileResource[+E2] <: Resource[E2]] extends Resource[E] with FileSystemResource[E, FileResource] {
  def contents: Stream[E, DirectoryEntry[E, FileResource]]

  def numEntries: Option[ZIO[Any, E, BigInt]] = None


  final def filterFiles(f: String => Boolean): DirectoryResource[E, FileResource] =
    new DirectoryResource[E, FileResource] {
      override def contents: Stream[E, DirectoryEntry[E, FileResource]] =
        DirectoryResource.this.contents
          .filter {
            case DirectoryEntry(_, fileName, _) => f(fileName)
          }

      override def numEntries: Option[ZIO[Any, E, BigInt]] =
        DirectoryResource.this.numEntries

      override def fileName: Option[String] =
        DirectoryResource.this.fileName
    }

}

object DirectoryResource {
  extension [E, FileResource[+E2] <: BinaryResource[E2]](dr: DirectoryResource[E, FileResource]) {
    def decode[Res[+E2] <: BinaryResource[E2]]: DirectoryResourceDecodePartiallyApplied[E, FileResource, Res] =
      DirectoryResourceDecodePartiallyApplied(dr)
  }

  final case class DirectoryResourceDecodePartiallyApplied[E, Res1[+E2] <: BinaryResource[E2], Res2[+E2] <: BinaryResource[E2]](res: DirectoryResource[E, Res1]) extends AnyVal {
    def withError[E2 >: E](using decoder: BinaryResourceDecoder[Res2, E2]): DirectoryResource[E2, Res2] =
      new DirectoryResource[E2, Res2] {
        override def contents: Stream[E2, DirectoryEntry[E2, Res2]] =
          res.contents.map {
            case DirectoryEntry(dirs, fileName, resource) =>
              DirectoryEntry(dirs, fileName, resource.decode[Res2])
          }

        override def fileName: Option[String] =
          res.fileName
      }
  }

  object DirectoryResourceDecodePartiallyApplied {
    given [E, E2 >: E, Res1[+E3] <: BinaryResource[E3], Res2[+E3] <: BinaryResource[E3]] => (decoder: BinaryResourceDecoder[Res2, E2]) => Conversion[DirectoryResourceDecodePartiallyApplied[E, Res1, Res2], DirectoryResource[E2, Res2]]:
      override def apply(x: DirectoryResourceDecodePartiallyApplied[E, Res1, Res2]): DirectoryResource[E2, Res2] =
        x.withError[E2]
    end given
  }
}
