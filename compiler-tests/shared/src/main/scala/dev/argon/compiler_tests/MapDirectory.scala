package dev.argon.compiler_tests

import dev.argon.util.{*, given}
import dev.argon.io.BinaryResource
import java.io.IOException
import dev.argon.io.DirectoryResource
import dev.argon.io.DirectoryEntry
import zio.stream.ZStream
import dev.argon.io.TextResource
import java.nio.charset.CharacterCodingException

final class MapDirectory(files: Map[String, String]) {

  private final class LibDir(prefix: String) extends DirectoryResource[CharacterCodingException, BinaryResource] {

    override def fileName: Option[String] =
      prefix.split("/").nn
        .view
        .map(_.nn)
        .filter(_.nonEmpty)
        .lastOption
        

    override def contents: ZStream[Any, CharacterCodingException, DirectoryEntry[CharacterCodingException, BinaryResource]] =
      ZStream.fromIterable(
        files
          .iterator
          .filter { (path, _) => path.startsWith(prefix) }
          .toSeq
          .groupBy { (path, _) =>
            val index = path.indexOf("/", prefix.length())
            if index >= 0 then
              Some(path.substring(0, index + 1).nn)
            else
              None
          }
          .flatMap {
            case (Some(prefix), _) =>
              Seq(DirectoryEntry.Subdirectory(prefix.substring(prefix.length()).nn, LibDir(prefix)))

            case (None, files) =>
              files.map { (path, content) =>
                val name = path.substring(prefix.length()).nn
                DirectoryEntry.File(name, LibFile(name, content))
              }
          }
      )
  }

  private final class LibFile(name: String, content: String) extends TextResource[CharacterCodingException] with TextResource.Impl[CharacterCodingException] {
    override def fileName: Option[String] = Some(name)
    override def asText: ZStream[Any, CharacterCodingException, String] = ZStream(content)
  }

  def directoryResource: DirectoryResource[CharacterCodingException, BinaryResource] =
    LibDir("/")

}

