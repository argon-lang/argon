package dev.argon.io

import zio.stream.*


abstract class DirectoryResource[-R, +E, +FileResource[-R2, +E2] <: Resource[R2, E2]] extends Resource[R, E] {
  def contents: ZStream[R, E, DirectoryEntry[R, E, FileResource]]
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
