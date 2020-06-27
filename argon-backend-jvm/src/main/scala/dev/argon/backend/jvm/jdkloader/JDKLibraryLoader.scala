package dev.argon.backend.jvm.jdkloader

import java.io.{IOException, InputStream}
import java.net.URI
import java.nio.file.{FileSystems, Files, Path, Paths}

import zio.stream._
import zio._
import zio.system.System
import cats.implicits._
import dev.argon.compiler.{Compilation, ErrorList}
import zio.blocking.Blocking
import zio.interop.catz.core._

@SuppressWarnings(Array("org.wartremover.warts.All"))
object JDKLibraryLoader {

  def systemLibraries(version: Int): ZStream[System with Blocking, Throwable, JavaLibrary] =
    ZStream.unwrap(
      JavaVersionHandler.isCurrentJDKVersion(version).flatMap {
        case true => IO.succeed(jrtLibs)
        case false =>
          system.property("java.home").map {
            case Some(javaHome) =>
              loadCtSymLibs(Paths.get(javaHome).resolve("lib").resolve("ct.sym"), version)

            case None => Stream.empty
          }
      }
    )

  private def jrtLibs: ZStream[Blocking, Throwable, JavaLibrary] =
    ZStream.unwrap(
      for {
        fs <- IO.effect { FileSystems.newFileSystem(URI.create("jrt:/"), new java.util.HashMap[String, Any]()) }
        rootDir <- IO.effect { fs.getRootDirectories.iterator.next }

        modulesDir = rootDir.resolve("modules")

      } yield DirectoryStreamHelpers.toZStream(Files.newDirectoryStream(modulesDir))
        .filterM { p => IO.succeed { Files.isDirectory(p) } }
        .mapM { moduleDir =>
          for {
            (moduleName, packages) <- ModuleInfoLoader.getModuleInfo(moduleDir.resolve("module-info.sym"))
          } yield new JrtModule(moduleName, moduleDir, packages)
        }
    )


  private def loadCtSymLibs(ctSym: Path, jdkVersion: Int): ZStream[Blocking, Throwable, JavaLibrary] =
    ZStream.unwrap(
      for {
        fs <- IO.effect { FileSystems.newFileSystem(ctSym, null : ClassLoader) }
        rootDir <- IO.effect { fs.getRootDirectories.iterator.next }

        versionCode = if(jdkVersion < 10) jdkVersion.toString else ('A' + (jdkVersion - 10)).toChar.toString

        modulesDir <- IO.effect {
          Some(rootDir.resolve(versionCode + "-modules")).filter(Files.isDirectory(_))
        }

        classDirs <- DirectoryStreamHelpers.toZStream(Files.newDirectoryStream(rootDir))
          .filter { p => p.getFileName.toString.contains("-") }
          .filterM { p => IO.succeed { Files.isDirectory(p) } }
          .runCollect

        _ <- IO.when(classDirs.isEmpty)(IO.fail(new Exception("No system libraries found")))

      } yield modulesDir.map(loadCtSymModules(classDirs)).getOrElse(Stream(new CtSymClassPath(classDirs)))
    )

  private def loadCtSymModules(classDirs: Seq[Path])(modulesDir: Path): ZStream[Blocking, Throwable, JavaLibrary] =
    DirectoryStreamHelpers.toZStream(Files.newDirectoryStream(modulesDir))
      .mapM(loadCtSymModule(classDirs))

  private def loadCtSymModule(classDirs: Seq[Path])(moduleDir: Path): RIO[Blocking, JavaLibrary] = for {
    (moduleName, packages) <- ModuleInfoLoader.getModuleInfo(moduleDir.resolve("module-info.sym"))
  } yield new CtSymModule(moduleName, classDirs, packages)

  private trait CtSymJavaLibrary {
    val classDirs: Seq[Path]

    def findClass(pkg: String, name: String): ZManaged[Blocking, Option[ErrorList], InputStream] = {
      val pkgDir = pkg.split('.')

      ZManaged.fromEffect(
        Stream.fromIterable(classDirs)
          .map { classDir => pkgDir.foldLeft(classDir)(_.resolve(_)).resolve(name + ".sym") }
          .filterM { classFile => IO.effect { Files.exists(classFile) } }
          .runHead
      )
        .foldM(
          failure = {
            case ex: IOException => ZManaged.fail(Some(Compilation.errorListForIOException(ex)))
            case ex => ZManaged.die(ex)
          },
          success = {
            case Some(path) =>
              ZManaged.fromAutoCloseable(ZIO.accessM[Blocking](_.get.effectBlocking {
                Files.newInputStream(path)
              }))
                .refineOrDie {
                  case ex: IOException => Some(Compilation.errorListForIOException(ex))
                }

            case None => ZManaged.fail(None)
          }
        )
    }
  }

  private final class CtSymModule(val moduleName: String, val classDirs: Seq[Path], packages: Set[String]) extends JavaModule with CtSymJavaLibrary {
    override def findClass(pkg: String, name: String): ZManaged[Blocking, Option[ErrorList], InputStream] =
      if(packages.contains(name))
        super.findClass(pkg, name)
      else
        Managed.fail(None)
  }

  private final class CtSymClassPath(val classDirs: Seq[Path]) extends ClassPath with CtSymJavaLibrary

  private final class JrtModule(val moduleName: String, moduleDir: Path, packages: Set[String]) extends JavaModule {
    override def findClass(pkg: String, name: String): ZManaged[Blocking, Option[ErrorList], InputStream] = {
      val pkgDir = pkg.split('.')

      pkgDir.headOption match {
        case Some("META-INF") => Managed.fail(None)
        case _ if name.contains('-') => Managed.fail(None)
        case _ =>
          val classFile = pkgDir.foldLeft(moduleDir)(_.resolve(_)).resolve(name + ".class")

          Managed.fromEffect(
            ZIO.ifM(ZIO.accessM[Blocking](_.get.effectBlocking { Files.exists(classFile) }.mapError(Some.apply)))(IO.succeed(classFile), IO.fail(None))
          )
            .flatMap { classFile =>
              ZManaged.fromAutoCloseable(ZIO.accessM[Blocking](_.get.effectBlocking {
                Files.newInputStream(classFile)
              }))
                .mapError(Some.apply)
            }
            .flatMapError {
              case Some(ex: IOException) => Managed.succeed(Some(Compilation.errorListForIOException(ex)))
              case Some(ex) => Managed.die(ex)
              case None => Managed.succeed(None)
            }

      }
    }

  }

}
