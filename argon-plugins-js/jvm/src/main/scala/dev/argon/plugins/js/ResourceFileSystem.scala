package dev.argon.plugins.js

import org.graalvm.polyglot.io.FileSystem

import java.net.{URI, URL}
import java.nio.channels.SeekableByteChannel
import java.nio.file.{AccessMode, DirectoryStream, LinkOption, NoSuchFileException, OpenOption, Path, Files}
import java.nio.file.attribute.FileAttribute
import java.util.{Set as JSet, Map as JMap, HashMap as JHashMap}


private[js] final class ResourceFileSystem extends FileSystem {

  private def getFileURL(path: Path): URL = {
    val url = classOf[ResourceFileSystem].getResource("/dev/argon/plugins/js/" + path)
    if url == null then
      throw new NoSuchFileException(path.toString)
    else
      url
  }

  override def parsePath(uri: URI): Path = Path.of(uri)

  override def parsePath(path: String): Path = Path.of(path)

  override def checkAccess(path: Path, modes: JSet[_ <: AccessMode], linkOptions: LinkOption*): Unit = {
    val _ = getFileURL(path)
  }

  override def createDirectory(dir: Path, attrs: FileAttribute[_]*): Unit =
    throw new UnsupportedOperationException()

  override def delete(path: Path): Unit =
    throw new UnsupportedOperationException()

  override def newByteChannel(path: Path, options: JSet[_ <: OpenOption], attrs: FileAttribute[_]*): SeekableByteChannel =
    Files.newByteChannel(Path.of(getFileURL(path).toURI))

  override def newDirectoryStream(dir: Path, filter: DirectoryStream.Filter[_ >: Path]): DirectoryStream[Path] =
    throw new UnsupportedOperationException()

  override def toAbsolutePath(path: Path): Path =
    path

  override def toRealPath(path: Path, linkOptions: LinkOption*): Path =
    path

  override def readAttributes(path: Path, attributes: String, options: LinkOption*): JMap[String, AnyRef] =
    JHashMap()
}
