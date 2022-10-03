package dev.argon.util.test

import dev.argon.io.*
import zio.stream.ZStream

import java.nio.charset.CharacterCodingException
import scala.quoted.*

enum CompileTimeFileSystem {
  case Directory(entries: Map[String, CompileTimeFileSystem])
  case File(data: String)


  def toDirectoryEntry(path: String, entryName: String): DirectoryEntry[Any, CharacterCodingException, TextResource] =
    this match {
      case CompileTimeFileSystem.Directory(entries) =>
        DirectoryEntry.Subdirectory(entryName, new DirectoryResource[Any, CharacterCodingException, TextResource] {
          override def contents: ZStream[Any, CharacterCodingException, DirectoryEntry[Any, CharacterCodingException, TextResource]] =
            ZStream.fromIterable(
              entries
                .toSeq
                .map { case (name, fs) =>
                  fs.toDirectoryEntry(path + "/" + name, name)
                }
            )

          override def fileName: Option[String] = Some(path)
        })

      case CompileTimeFileSystem.File(data) =>
        DirectoryEntry.File(entryName, new TextResource[Any, CharacterCodingException] with TextResource.Impl[Any, CharacterCodingException] {
          override def asText: ZStream[Any, CharacterCodingException, String] =
            ZStream.succeed(data)

          override def fileName: Option[String] = Some(path)
        })
    }
}
