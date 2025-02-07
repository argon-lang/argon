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
  given [E]: BinaryResourceDecoder[BinaryResource, E] with
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


  final def flatten: Stream[E, (String, FileResource[E])] =
    contents.flatMap {
      case DirectoryEntry.Subdirectory(name, resource) =>
        resource.flatten.map { (subName, res) =>
          name + "/" + subName -> res
        }

      case DirectoryEntry.File(name, resource) =>
        ZStream(name -> resource)
    }

  final def filterFiles(f: String => Boolean): DirectoryResource[E, FileResource] =
    new DirectoryResource[E, FileResource] {
      override def contents: Stream[E, DirectoryEntry[E, FileResource]] =
        DirectoryResource.this.contents
          .map {
            case DirectoryEntry.Subdirectory(name, resource) => DirectoryEntry.Subdirectory(name, resource.filterFiles(f))
            case entry => entry
          }
          .filter {
            case DirectoryEntry.File(name, _) => f(name)
            case _ => true
          }

      override def numEntries: Option[ZIO[Any, E, BigInt]] =
        DirectoryResource.this.numEntries

      override def fileName: Option[String] =
        DirectoryResource.this.fileName
    }

}

object DirectoryResource {
  def decode[E, FileResource[+E2] <: BinaryResource[E2], Res[+E2] <: Resource[E2]](dr: DirectoryResource[E, FileResource])(using BinaryResourceDecoder[Res, E]): DirectoryResource[E, Res] =
    new DirectoryResource[E, Res] {
      override def contents: Stream[E, DirectoryEntry[E, Res]] =
        dr.contents.map {
          case DirectoryEntry.Subdirectory(name, resource) => DirectoryEntry.Subdirectory(name, decode[E, FileResource, Res](resource))
          case DirectoryEntry.File(name, resource) => DirectoryEntry.File(name, summon[BinaryResourceDecoder[Res, E]].decode(resource))
        }

      override def fileName: Option[String] =
        dr.fileName
    }

}
