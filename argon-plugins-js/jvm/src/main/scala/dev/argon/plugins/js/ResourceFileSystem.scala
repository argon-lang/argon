package dev.argon.plugins.js

import org.graalvm.polyglot.io.FileSystem

import java.net.{URI, URL}
import java.nio.channels.SeekableByteChannel
import java.nio.file.{AccessMode, DirectoryStream, FileSystemNotFoundException, Files, LinkOption, NoSuchFileException, OpenOption, Path}
import java.nio.file.attribute.FileAttribute
import java.nio.file.spi.FileSystemProvider
import java.util.{HashMap as JHashMap, Map as JMap, Set as JSet}
import scala.jdk.CollectionConverters.*
import dev.argon.util.{*, given}


private[js] final class ResourceFileSystem extends FileSystem {

  private def getFileURL(path: Path): URL = {
    val url = classOf[ResourceFileSystem].getResource("/dev/argon/plugins/js/" + path)
    if url == null then
      throw new NoSuchFileException(path.toString)
    else
      url
  }

  override def parsePath(uri: URI): Path = Path.of(uri).nn

  override def parsePath(path: String): Path = Path.of(path).nn

  override def checkAccess(path: Path, modes: JSet[? <: AccessMode], linkOptions: LinkOption*): Unit = {
    val _ = getFileURL(path)
  }

  override def createDirectory(dir: Path, attrs: FileAttribute[?]*): Unit =
    throw new UnsupportedOperationException()

  override def delete(path: Path): Unit =
    throw new UnsupportedOperationException()

  override def newByteChannel(path: Path, options: JSet[? <: OpenOption], attrs: FileAttribute[?]*): SeekableByteChannel =
    val uri = getFileURL(path).toURI.nn

    if uri.getScheme == "jar" then
      FileSystemProvider.installedProviders().nn
        .asScala
        .find { _.getScheme.nn.equalsIgnoreCase("jar") }
        .foreach { provider =>
          try provider.getFileSystem(uri)
          catch {
            case _: FileSystemNotFoundException =>
              provider.newFileSystem(uri, java.util.Collections.emptyMap())
          }
        }
    end if

    Files.newByteChannel(Path.of(uri)).nn
  end newByteChannel

  override def newDirectoryStream(dir: Path, filter: DirectoryStream.Filter[? >: Path]): DirectoryStream[Path] =
    throw new UnsupportedOperationException()

  override def toAbsolutePath(path: Path): Path =
    path

  override def toRealPath(path: Path, linkOptions: LinkOption*): Path =
    path

  override def readAttributes(path: Path, attributes: String, options: LinkOption*): JMap[String, AnyRef] =
    JHashMap()
}
