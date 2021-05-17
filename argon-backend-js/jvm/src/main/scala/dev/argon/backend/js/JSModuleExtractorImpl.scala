package dev.argon.backend.js

import java.io.FileNotFoundException
import java.net.{URI, URL}
import java.nio.channels.SeekableByteChannel
import java.nio.file.attribute.FileAttribute

import org.apache.commons.io.{FilenameUtils, IOUtils}
import org.apache.commons.io.input.NullInputStream
import org.apache.commons.io.output.NullOutputStream
import org.graalvm.polyglot.io.FileSystem
import org.graalvm.polyglot.{Context, Source}
import zio.blocking.Blocking
import zio.Task
import java.nio.file.{AccessMode, DirectoryStream, LinkOption, OpenOption, Path}
import java.util

import dev.argon.backend.js.JSModuleExtractorImpl.ResourceFileSystem
import org.apache.commons.compress.utils.SeekableInMemoryByteChannel

import scala.annotation.unused
import scala.jdk.CollectionConverters._

private[js] final case class JSModuleExtractorImpl(blocking: Blocking.Service) extends JSModuleExtractor {

  @SuppressWarnings(Array("dev.argon.warts.ZioEffect"))
  private[js] def exportedFunctions(module: String): Task[Map[String, String]] =
    blocking.effectBlockingInterrupt {
      val context = Context.newBuilder("js")
        .out(NullOutputStream.NULL_OUTPUT_STREAM)
        .err(NullOutputStream.NULL_OUTPUT_STREAM)
        .in(new NullInputStream(0))
        .allowIO(true)
        .fileSystem(new ResourceFileSystem("dev/argon/js_module_extractor/"))
        .build()
      try {
        val mainSource = Source.newBuilder("js",
          """
            |import { extractModuleFunctions } from "./js-module-extractor.mjs"
            |
            |extractModuleFunctions
            |""".stripMargin,
          "main.mjs"
        )
          .mimeType("application/javascript+module")
          .build()

        val extractModuleFunctions = context.eval(mainSource)

        val exportedFunctions = extractModuleFunctions.execute(module)

        exportedFunctions.getMemberKeys
          .asScala
          .toSeq
          .map { name => name -> exportedFunctions.getMember(name).asString() }
          .toMap
      }
      finally context.close()
    }

}

object JSModuleExtractorImpl {

  @SuppressWarnings(Array("scalafix:DisableSyntax.==", "scalafix:DisableSyntax.null", "scalafix:Disable.toString"))
  final class ResourceFileSystem(resourcePath: String) extends FileSystem {

    private def getResURL(path: Path): URL = {
      val norm = FilenameUtils.normalize(Path.of(resourcePath).resolve(path).toString)
      if(norm == null || !norm.startsWith(resourcePath)) {
        throw new FileNotFoundException()
      }

      val url = getClass.getClassLoader.getResource(norm)
      if(url == null) {
        throw new FileNotFoundException()
      }

      url
    }

    override def parsePath(uri: URI): Path = Path.of(uri)

    override def parsePath(path: String): Path = Path.of(path)

    override def checkAccess(path: Path, @unused modes: util.Set[_ <: AccessMode], @unused linkOptions: LinkOption*): Unit = {
      val _ = getResURL(path)
    }

    override def createDirectory(@unused dir: Path, @unused attrs: FileAttribute[_]*): Unit =
      throw new UnsupportedOperationException()

    override def delete(path: Path): Unit =
      throw new UnsupportedOperationException()

    override def newByteChannel(path: Path, @unused options: util.Set[_ <: OpenOption], @unused attrs: FileAttribute[_]*): SeekableByteChannel = {
      val url = getResURL(path)
      val content = IOUtils.toByteArray(url)
      new SeekableInMemoryByteChannel(content)
    }

    override def newDirectoryStream(dir: Path, filter: DirectoryStream.Filter[_ >: Path]): DirectoryStream[Path] =
      throw new UnsupportedOperationException()

    override def toAbsolutePath(path: Path): Path =
      Path.of("/").resolve(path)

    override def toRealPath(@unused path: Path, @unused linkOptions: LinkOption*): Path = {
      val norm = FilenameUtils.normalize(Path.of(resourcePath).resolve(path).toString)
      if(norm == null || !norm.startsWith(resourcePath)) {
        throw new FileNotFoundException()
      }

      Path.of(norm.substring(resourcePath.length) match {
        case "acorn" => "node_modules/acorn/dist/acorn.mjs"
        case p => p
      })
    }

    override def readAttributes(@unused path: Path, @unused attributes: String, @unused options: LinkOption*): util.Map[String, AnyRef] =
      throw new UnsupportedOperationException()
  }

}
