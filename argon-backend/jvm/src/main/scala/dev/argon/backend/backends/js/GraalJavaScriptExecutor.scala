package dev.argon.backend.backends.js

import org.graalvm.polyglot.*

import scala.collection.mutable
import org.graalvm.polyglot.io.{FileSystem, IOAccess}

import java.io.{ByteArrayOutputStream, FileNotFoundException, IOException, InputStream}
import java.net.URI
import java.nio.channels.SeekableByteChannel
import java.nio.charset.StandardCharsets
import java.nio.file.attribute.FileAttribute
import java.nio.file.{AccessMode, DirectoryStream, LinkOption, OpenOption, Path}
import dev.argon.util.*
import org.apache.commons.compress.utils.SeekableInMemoryByteChannel

import scala.jdk.CollectionConverters.given

@HostAccess.Export
class GraalJavaScriptExecutor @HostAccess.Export() extends AutoCloseable {
  private val outputStream = new ByteArrayOutputStream()

  private val fileSystem = MemoryFileSystem()

  private val context: Context =
    Context.newBuilder("js")
      //          .option("js.load", "false")
      //          .option("js.print", "false")
      .option("js.esm-eval-returns-exports", "true")
      // .option("js.text-encoding", "true")
      .option("engine.WarnInterpreterOnly", "false")
      .allowIO(
        IOAccess.newBuilder()
          .fileSystem(fileSystem)
          .build()
      )
      .out(outputStream)
//      .err(outputStream)
      .in(InputStream.nullInputStream())
      .build()

  @HostAccess.Export
  def addFile(path: String, contents: String): Unit =
    fileSystem.fileStore(path) = contents


  @HostAccess.Export
  def executeScript(code: String): Unit =
    val script = Source.newBuilder("js", code, null).build()
    context.eval(script)
  end executeScript

  @HostAccess.Export
  def executeModule(code: String): Unit =
    val module = Source.newBuilder("js", code, "main.js")
      .mimeType("application/javascript+module")
      .build()
    try context.eval(module)
    catch {
      case ex: Throwable =>
        println(fileSystem.fileStore)
        throw ex
    }
  end executeModule

  @HostAccess.Export
  def output(): String =
    outputStream.toString(StandardCharsets.UTF_8)

  @HostAccess.Export
  override def close(): Unit =
    context.close()


  private class MemoryFileSystem() extends FileSystem {
    val fileStore = mutable.Map[String, String]()

    override def parsePath(uri: URI): Path = Path.of(uri)
    override def parsePath(path: String): Path = Path.of(path)

    override def checkAccess(path: Path, modes: java.util.Set[? <: AccessMode], linkOptions: LinkOption*): Unit =
      if !fileStore.contains(getFullPath(path)) && !isDirectory(path) then
        throw new FileNotFoundException()
    end checkAccess

    override def createDirectory(dir: Path, attrs: FileAttribute[?]*): Unit =
      throw new IOException("File system is read only")

    override def delete(path: Path): Unit =
      throw new IOException("File system is read only")

    override def newByteChannel(path: Path, options: java.util.Set[? <: OpenOption], attrs: FileAttribute[?]*): SeekableByteChannel =
      val content = fileStore.getOrElse(getFullPath(path), throw new FileNotFoundException())
      new SeekableInMemoryByteChannel(content.getBytes(StandardCharsets.UTF_8))
    end newByteChannel

    override def newDirectoryStream(dir: Path, filter: DirectoryStream.Filter[? >: Path]): DirectoryStream[Path] =
      throw new UnsupportedOperationException()

    override def toAbsolutePath(path: Path): Path =
      Path.of("/").resolve(path)

    override def toRealPath(path: Path, linkOptions: LinkOption*): Path =
      path

    override def readAttributes(path: Path, attributes: String, options: LinkOption*): java.util.Map[String, AnyRef] =
      java.util.Map.of()

    private def getFullPath(path: Path): String =
      "/" + path.asScala.mkString("/")

    private def isDirectory(path: Path): Boolean =
      val dir = getFullPath(path) + "/"
      fileStore.keysIterator.exists(_.startsWith(dir))
    end isDirectory
  }

}
