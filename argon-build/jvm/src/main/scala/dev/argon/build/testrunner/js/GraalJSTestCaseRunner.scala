package dev.argon.build.testrunner.js

import dev.argon.build.BuildEnvironment
import dev.argon.io.Path
import zio._
import cats._
import cats.implicits._
import org.graalvm.polyglot.Context
import org.graalvm.polyglot.Value
import javax.script.ScriptEngineManager
import javax.script.ScriptEngine
import javax.script.Invocable
import java.io.{ByteArrayOutputStream, FileNotFoundException, IOException}
import java.net.URI
import java.nio.channels.SeekableByteChannel
import java.nio.charset.StandardCharsets
import java.nio.file
import java.nio.file.{AccessMode, DirectoryStream, LinkOption, OpenOption}
import java.nio.file.attribute.FileAttribute
import java.util
import java.util.Locale

import org.apache.commons.compress.utils.SeekableInMemoryByteChannel
import org.apache.commons.io.input.NullInputStream
import org.apache.commons.text.StringEscapeUtils
import org.graalvm.polyglot.Source
import org.graalvm.polyglot.io.FileSystem

final class GraalJSTestCaseRunner(references: UIO[Vector[Path]]) extends JavaScriptTestCaseRunnerBase(references) {
  override protected def executeJS(compiledFile: String)(modules: Seq[FileInfo]): ZIO[BuildEnvironment, Throwable, String] =
    IO.effect {

      val output = new ByteArrayOutputStream()

      val context = Context.newBuilder("js")
        .allowIO(true)
        .out(output)
        .err(output)
        .in(new NullInputStream(0))
        .fileSystem(new JSFileSystem(modules))
        .build()
      try {
        val mainSource = Source.newBuilder("js",
          s"""
             |import * as arCore from "Argon.Core";
             |import * as mainModule from "${StringEscapeUtils.escapeEcmaScript(moduleName)}";
             |
             |mainModule.functions["main:(Ar.Unit)->(Ar.Unit)"].value(arCore.unitValue)
             |""".stripMargin,
          "main.mjs"
        )
          .mimeType("application/javascript+module")
          .build()

        val _ = context.eval(mainSource)

        output.toString(StandardCharsets.UTF_8)
      }
      catch {
        case ex: Exception =>
          println(ex)
          throw ex
      }
      finally context.close()
    }

  @SuppressWarnings(Array("org.wartremover.warts.ToString"))
  class JSFileSystem(modules: Seq[FileInfo]) extends FileSystem {

    private def getFileInfo(path: file.Path): FileInfo =
      modules.find(f => f.name + ".mjs" === path.toString).getOrElse {
        println(s"Not found: ${path.toString}, ${modules.toString()}")
        throw new FileNotFoundException()
      }

    override def parsePath(uri: URI): file.Path = file.Path.of(uri)

    override def parsePath(path: String): file.Path = file.Path.of(path)

    override def checkAccess(path: file.Path, modes: util.Set[_ <: AccessMode], linkOptions: LinkOption*): Unit = {
      val _ = getFileInfo(path)
    }

    override def createDirectory(dir: file.Path, attrs: FileAttribute[_]*): Unit =
      throw new UnsupportedOperationException()

    override def delete(path: file.Path): Unit =
      throw new UnsupportedOperationException()

    override def newByteChannel(path: file.Path, options: util.Set[_ <: OpenOption], attrs: FileAttribute[_]*): SeekableByteChannel = {
      val bytes = getFileInfo(path).content.getBytes(StandardCharsets.UTF_8)
      new SeekableInMemoryByteChannel(bytes)
    }

    override def newDirectoryStream(dir: file.Path, filter: DirectoryStream.Filter[_ >: file.Path]): DirectoryStream[file.Path] =
      throw new UnsupportedOperationException()

    override def toAbsolutePath(path: file.Path): file.Path = path

    override def toRealPath(path: file.Path, linkOptions: LinkOption*): file.Path = {
      val p = path.normalize()
      if(p.toString.toUpperCase(Locale.US).endsWith(".MJS"))
        p
      else
        file.Path.of(p.toString + ".mjs")
    }

    override def readAttributes(path: file.Path, attributes: String, options: LinkOption*): util.Map[String, AnyRef] =
      throw new UnsupportedOperationException()
  }

}
