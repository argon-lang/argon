package dev.argon.plugins.js

import dev.argon.plugins.js.estree.ImportDeclaration
import dev.argon.util.{*, given}
import zio.*

import scala.jdk.CollectionConverters.*
import java.io.ByteArrayOutputStream
import java.nio.channels.SeekableByteChannel
import java.nio.charset.StandardCharsets
import java.net.URI
import java.nio.file.attribute.FileAttribute
import java.nio.file.{AccessMode, DirectoryStream, LinkOption, OpenOption, Path}
import java.util.{HashMap as JHashMap, Map as JMap, Set as JSet}
import org.apache.commons.io.input.NullInputStream
import org.apache.commons.compress.utils.SeekableInMemoryByteChannel
import org.apache.commons.io.FilenameUtils
import org.graalvm.polyglot.Context
import org.graalvm.polyglot.io.FileSystem
import org.graalvm.polyglot.Source


object JSOutputExecutor {

  def run(fileSystem: Map[String, String]): Task[String] =
    fixImportPathsFS(fileSystem).flatMap { fileSystem =>
      ZIO.attempt {
        val output = new ByteArrayOutputStream()

        val context = Context.newBuilder("js")
          .allowIO(true)
          .out(output)
          .err(output)
          .in(new NullInputStream(0))
          .fileSystem(new JSFileSystem(fileSystem))
          .option("engine.WarnInterpreterOnly", false.toString)
          .build()
        try {
          val mainSource = Source.newBuilder("js",
            """
              |import * as argonRuntime from "/argon/node_modules/@argon-lang/runtime/lib/index.js";
              |import { main$p$z$r$z } from "/argon/node_modules/@tubes/Test/lib/index.js";
              |
              |await argonRuntime.trampoline.resolve(main$p$z$r$z([]));
              |""".stripMargin,
            "/argon/main.mjs"
          )
            .mimeType("application/javascript+module")
            .build()

          val _ = context.eval(mainSource)

          output.toString(StandardCharsets.UTF_8)
        }
        finally context.close()
      }
    }

  private def fixImportPathsFS(fileSystem: Map[String, String]): Task[Map[String, String]] =
    val moduleResolution = ModuleResolution(fileSystem)
    ZIO.foreach(fileSystem) { (name, content) =>
      if name.endsWith(".js") then
        fixImportPaths(moduleResolution, name, content).map { name -> _ }
      else
        ZIO.succeed(name -> content)
    }

  private def fixImportPaths(moduleResolution: ModuleResolution, name: String, content: String): Task[String] =
    ZIO.scoped(
      for
        context <- JSContext.make
        program <- context.parse(name, content)
        fixed <- context.generate(fixImportPathsAST(moduleResolution, name, program))
      yield fixed
    )

  private def fixImportPathsAST(moduleResolution: ModuleResolution, name: String, program: estree.Program): estree.Program =
    if program.sourceType == "module" then
      estree.Program(
        sourceType = "module",
        body = program.body.map {
          case importDecl: ImportDeclaration =>
            importDecl.source.value.unwrap match {
              case specifier: String =>
                val resolvedSpecifier = moduleResolution.resolveSpecifier(specifier, name)
                estree.ImportDeclaration(
                  specifiers = importDecl.specifiers,
                  source = estree.Literal(value = Nullable(resolvedSpecifier))
                )

              case _ => importDecl
            }

          case stmt => stmt
        }
      )
    else
      program
    end if

  private final class JSFileSystem(fs: Map[String, String]) extends FileSystem {
    override def parsePath(uri: URI): Path = Path.of(uri)
    override def parsePath(path: String): Path = Path.of(path)

    override def checkAccess(path: Path, modes: JSet[? <: AccessMode], linkOptions: LinkOption*): Unit = ()

    override def createDirectory(dir: Path, attrs: FileAttribute[?]*): Unit =
      throw new UnsupportedOperationException()

    override def delete(path: Path): Unit =
      throw new UnsupportedOperationException()

    override def newByteChannel(path: Path, options: JSet[? <: OpenOption], attrs: FileAttribute[?]*): SeekableByteChannel =
      val name = path.toString
      val content = fs(name).getBytes(StandardCharsets.UTF_8)
      new SeekableInMemoryByteChannel(content)
    end newByteChannel


    override def newDirectoryStream(dir: Path, filter: DirectoryStream.Filter[? >: Path]): DirectoryStream[Path] =
      throw new UnsupportedOperationException()

    override def toAbsolutePath(path: Path): Path =
      if !path.isAbsolute then
        Path.of("/" + path.toString)
      else
        path

    override def toRealPath(path: Path, linkOptions: LinkOption*): Path =
      toAbsolutePath(path)


    override def readAttributes(path: Path, attributes: String, options: LinkOption*): JMap[String, AnyRef] =
      JHashMap()
  }

}
