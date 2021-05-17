package dev.argon.build.testrunner.js

import zio._
import cats.implicits._
import org.graalvm.polyglot.Context
import java.io.{ByteArrayOutputStream, FileNotFoundException}
import java.net.URI
import java.nio.channels.SeekableByteChannel
import java.nio.charset.StandardCharsets
import java.nio.file
import java.nio.file.{AccessMode, DirectoryStream, LinkOption, OpenOption}
import java.nio.file.attribute.FileAttribute
import java.util
import java.util.Locale

import dev.argon.backend.js.JSBackend
import dev.argon.options.FileList
import org.apache.commons.compress.utils.SeekableInMemoryByteChannel
import org.apache.commons.io.input.NullInputStream
import org.apache.commons.text.StringEscapeUtils
import org.graalvm.polyglot.Source
import org.graalvm.polyglot.io.FileSystem

import scala.annotation.unused

final class GraalJSTestCaseRunner(protected val backend: JSBackend, protected val references: FileList, blocking: zio.blocking.Blocking.Service) extends JavaScriptTestCaseRunnerBase {

  @SuppressWarnings(Array("dev.argon.warts.ZioEffect"))
  override protected def executeJS(compiledFile: String)(modules: Seq[FileInfo]): Task[String] =
    blocking.effectBlockingInterrupt {

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
             |import arCore, { unitValue } from "Argon.Core";
             |import mainModule from "${StringEscapeUtils.escapeEcmaScript(moduleName)}";
             |
             |const unitType = { type: "class", arClass: arCore.globalClass(["Ar"], "Unit", { parameterTypes: [] }), arguments: [] };
             |mainModule.globalFunction([], "main", { parameterTypes: [unitType], resultType: unitType }).invoke(unitValue);
             |""".stripMargin,
          "main.mjs"
        )
          .mimeType("application/javascript+module")
          .build()

        val _ = context.eval(mainSource)

        output.toString(StandardCharsets.UTF_8)
      }
      finally context.close()
    }

  @SuppressWarnings(Array("scalafix:Disable.toString"))
  class JSFileSystem(modules: Seq[FileInfo]) extends FileSystem {

    private def getFileInfo(path: file.Path): FileInfo =
      modules.find(f => f.name + ".mjs" === path.toString).getOrElse {
        throw new FileNotFoundException()
      }

    override def parsePath(uri: URI): file.Path = file.Path.of(uri)

    override def parsePath(path: String): file.Path = file.Path.of(path)

    override def checkAccess(path: file.Path, @unused modes: util.Set[_ <: AccessMode], @unused linkOptions: LinkOption*): Unit = {
      val _ = getFileInfo(path)
    }

    override def createDirectory(@unused dir: file.Path, @unused attrs: FileAttribute[_]*): Unit =
      throw new UnsupportedOperationException()

    override def delete(@unused path: file.Path): Unit =
      throw new UnsupportedOperationException()

    override def newByteChannel(path: file.Path, @unused options: util.Set[_ <: OpenOption], @unused attrs: FileAttribute[_]*): SeekableByteChannel = {
      val bytes = getFileInfo(path).content.getBytes(StandardCharsets.UTF_8)
      new SeekableInMemoryByteChannel(bytes)
    }

    override def newDirectoryStream(@unused dir: file.Path, @unused filter: DirectoryStream.Filter[_ >: file.Path]): DirectoryStream[file.Path] =
      throw new UnsupportedOperationException()

    override def toAbsolutePath(@unused path: file.Path): file.Path = path

    override def toRealPath(path: file.Path, @unused linkOptions: LinkOption*): file.Path = {
      val p = path.normalize()
      if(p.toString.toUpperCase(Locale.US).endsWith(".MJS"))
        p
      else
        file.Path.of(p.toString + ".mjs")
    }

    override def readAttributes(@unused path: file.Path, @unused attributes: String, @unused options: LinkOption*): util.Map[String, AnyRef] =
      throw new UnsupportedOperationException()
  }

}
