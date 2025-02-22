package dev.argon.backend.backends.js

import org.graalvm.polyglot.*

import scala.collection.mutable
import org.graalvm.polyglot.io.{FileSystem, IOAccess}

import java.io.{ByteArrayOutputStream, InputStream}
import java.net.URI
import java.nio.channels.SeekableByteChannel
import java.nio.charset.StandardCharsets
import java.nio.file.attribute.FileAttribute
import java.nio.file.{AccessMode, DirectoryStream, LinkOption, OpenOption, Path}
import java.util

@HostAccess.Export
class GraalJavaScriptExecutor @HostAccess.Export() extends AutoCloseable {
  private val outputStream = new ByteArrayOutputStream()
  
  private val context: Context =
    Context.newBuilder("js")
      //          .option("js.load", "false")
      //          .option("js.print", "false")
      .option("js.esm-eval-returns-exports", "true")
      // .option("js.text-encoding", "true")
      .option("engine.WarnInterpreterOnly", "false")
      .allowIO(
        IOAccess.newBuilder()
          .fileSystem(MemoryFileSystem())
          .build()
      )
      .out(outputStream)
      .err(outputStream)
      .in(InputStream.nullInputStream())
      .build()

  private val fileStore = mutable.Map[Seq[String], String]()

  @HostAccess.Export
  def addFile(path: Array[String], contents: String): Unit =
    fileStore(path.toSeq) = contents


  @HostAccess.Export
  def executeScript(code: String): Unit =
    val script = Source.newBuilder("js", code, null).build()
    context.eval(script)
  end executeScript

  @HostAccess.Export
  def executeModule(code: String): Unit =
    val module = Source.newBuilder("js", code, null)
      .mimeType("application/javascript+module")
      .build()
    context.eval(module)
  end executeModule

  @HostAccess.Export
  def output(): String =
    outputStream.toString(StandardCharsets.UTF_8)

  @HostAccess.Export
  override def close(): Unit =
    context.close()


  private class MemoryFileSystem extends FileSystem {
    override def parsePath(uri: URI): Path = Path.of(uri)
    override def parsePath(path: String): Path = Path.of(path)

    override def checkAccess(path: Path, modes: util.Set[? <: AccessMode], linkOptions: LinkOption*): Unit = ???

    override def createDirectory(dir: Path, attrs: FileAttribute[?]*): Unit = ???

    override def delete(path: Path): Unit = ???

    override def newByteChannel(path: Path, options: util.Set[? <: OpenOption], attrs: FileAttribute[?]*): SeekableByteChannel = ???

    override def newDirectoryStream(dir: Path, filter: DirectoryStream.Filter[? >: Path]): DirectoryStream[Path] = ???

    override def toAbsolutePath(path: Path): Path = ???

    override def toRealPath(path: Path, linkOptions: LinkOption*): Path = ???

    override def readAttributes(path: Path, attributes: String, options: LinkOption*): util.Map[String, AnyRef] = ???
  }

}
