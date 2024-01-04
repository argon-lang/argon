package dev.argon.io

import zio.*
import zio.stream.*

import java.io.IOException
import java.nio.charset.CharacterCodingException

abstract class Resource[-R, +E] {
  def fileName: Option[String]
}

object Resource:
  trait WithoutFileName:
    def fileName: Option[String] = None
  end WithoutFileName
end Resource


abstract class BinaryResource[-R, +E] extends Resource[R, E] with BinaryResourcePlatformSpecific[R, E] {
  def asBytes: ZStream[R, E, Byte]

  def byteSize: Option[ZIO[R, E, BigInt]] = None
}

object BinaryResource:
  given BinaryResourceDecoder[BinaryResource, Any, Nothing] with
    def decode[R <: Any, E >: Nothing](resource: BinaryResource[R, E]): BinaryResource[R, E] =
      resource
  end given
end BinaryResource

sealed trait FileSystemResource[-R, +E, +FileResource[-R2, +E2] <: Resource[R2, E2]]

object FileSystemResource {
  final case class Of[-R, +E, +FileResource[-R2, +E2] <: Resource[R2, E2]](fileResource: FileResource[R, E]) extends FileSystemResource[R, E, FileResource]
}

abstract class DirectoryResource[-R, +E, +FileResource[-R2, +E2] <: Resource[R2, E2]] extends Resource[R, E] with FileSystemResource[R, E, FileResource] {
  def contents: ZStream[R, E, DirectoryEntry[R, E, FileResource]]

  def numEntries: Option[ZIO[R, E, BigInt]] = None


  final def flatten: ZStream[R, E, (String, FileResource[R, E])] =
    contents.flatMap {
      case DirectoryEntry.Subdirectory(name, resource) =>
        resource.flatten.map { (subName, res) =>
          name + "/" + subName -> res
        }

      case DirectoryEntry.File(name, resource) =>
        ZStream(name -> resource)
    }

}

object DirectoryResource {

  extension [R, E, FileResource[-R2, +E2] <: BinaryResource[R2, E2]](dr: DirectoryResource[R, E, FileResource])
    def decode[Res[-R2, +E2] <: Resource[R2, E2]](using BinaryResourceDecoder[Res, R, E]): DirectoryResource[R, E, Res] =
      new DirectoryResource[R, E, Res] {
        override def contents: ZStream[R, E, DirectoryEntry[R, E, Res]] =
          dr.contents.map {
            case DirectoryEntry.Subdirectory(name, resource) => DirectoryEntry.Subdirectory(name, resource.decode[Res])
            case DirectoryEntry.File(name, resource) => DirectoryEntry.File(name, summon[BinaryResourceDecoder[Res, R, E]].decode(resource))
          }

        override def fileName: Option[String] =
          dr.fileName
      }
  end extension


}
