package dev.argon.compiler_tests

import dev.argon.util.{*, given}
import dev.argon.io.*
import zio.stream.ZStream
import java.nio.charset.CharacterCodingException

final class MapDirectory(files: Map[String, String]) extends DirectoryResource[CharacterCodingException, TextResource] with Resource.WithoutFileName {

  override def contents: ZStream[Any, CharacterCodingException, DirectoryEntry[CharacterCodingException, TextResource]] =
    ZStream.fromIterable(files)
      .map { (fullName, content) =>
        val nameParts = fullName.split("\\/").nn.iterator.map(_.nn).toSeq

        val name = nameParts.last

        DirectoryEntry(nameParts.init.toSeq, name, MapDirectory.LibFile(name, content))
      }
}

object MapDirectory {
  private final class LibFile(name: String, content: String) extends TextResource[CharacterCodingException] with TextResource.Impl[CharacterCodingException] {
    override def fileName: Option[String] = Some(name)
    override def asText: ZStream[Any, CharacterCodingException, String] = ZStream(content)
  }
}

