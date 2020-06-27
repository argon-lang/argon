package dev.argon.backend.jvm.jdkloader

import java.io.{IOException, InputStream}
import java.nio.file.Path
import java.util.jar.JarFile
import java.util.zip.ZipFile

import zio._
import zio.system._
import java.lang.Runtime.{Version => JDKVersion}

import cats.data.NonEmptyList
import dev.argon.compiler._
import dev.argon.compiler.loaders.{ResourceIndicator, ResourceReader}
import dev.argon.io.JarFileReader
import org.apache.commons.io.FilenameUtils
import zio.blocking.Blocking

@SuppressWarnings(Array("dev.argon.warts.ZioEffect"))
object JarLibraryLoader {

  def loadJar[I <: ResourceIndicator: Tag](jar: I, jdkVersion: JDKVersion): ZManaged[ResourceReader[I], ErrorList, JavaLibrary] =
    ZManaged.accessManaged[ResourceReader[I]](_.get.getJarReader(jar, jdkVersion)).mapM { jarReader =>
      if(jdkVersion.feature() >= 9)
        for {
          (moduleName, packages) <- getModuleName(jar, jarReader)
        } yield new JarModule(moduleName, jarReader, packages)
      else
        IO.succeed(new JarClassPathEntry(jarReader))
    }

  private def getModuleName[I <: ResourceIndicator: Tag](jar: I, jarReader: JarFileReader[Any, ErrorList]): IO[ErrorList, (String, Option[Set[String]])] = {

    def moduleInfo: IO[Option[ErrorList], (String, Option[Set[String]])] =
      jarReader.getEntryStream("module-info.class")
        .mapError(Some.apply)
        .flatMap {
          case Some(entryStream) =>
            entryStream
              .mapError(WrappedErrorListException.toThrowable)
              .toInputStream
              .use(ModuleInfoLoader.getModuleInfo)
              .refineOrDie(WrappedErrorListException.fromThrowable)
              .mapError(Some.apply)
              .map { case (moduleName, packages) => (moduleName, Some(packages)) }

          case None => IO.fail(None)
        }

    def manifestInfo: IO[Option[ErrorList], (String, Option[Set[String]])] =
      jarReader.manifest
        .mapError(Some.apply)
        .flatMap(IO.fromOption(_))
        .flatMap { manifest =>
          IO.fromOption(Option(manifest.getMainAttributes.getValue("Automatic-Module-Name")))
        }
        .map { moduleName => (moduleName, None) }

    def fileNameInfo: IO[ErrorList, (String, Option[Set[String]])] =
      jarReader.jarName.map { moduleName =>
        (moduleName, None)
      }

    moduleInfo.orElseOptional(manifestInfo).catchAll {
      case Some(ex) => IO.fail(ex)
      case None => fileNameInfo
    }

  }

  trait JarLibrary {
    protected val jarReader: JarFileReader[Any, ErrorList]

    def findClass(pkg: String, name: String): ZManaged[Blocking, Option[ErrorList], InputStream] = {
      val entryPath = pkg.replace('.' , '/') + "/" + name + ".class"
      ZManaged.fromEffect(jarReader.getEntryStream(entryPath))
        .mapError(Some.apply)
        .flatMap {
          case Some(stream) =>
            stream
              .mapError(WrappedErrorListException.toThrowable)
              .toInputStream
              .refineOrDie(WrappedErrorListException.fromThrowable)
              .mapError(Some.apply)
          case None => ZManaged.fail(None)
        }
    }
  }

  final class JarModule(val moduleName: String, protected val jarReader: JarFileReader[Any, ErrorList], packages: Option[Set[String]]) extends JavaModule with JarLibrary {
    override def findClass(pkg: String, name: String): ZManaged[Blocking, Option[ErrorList], InputStream] =
      if(packages.forall(_.contains(pkg)))
        super.findClass(pkg, name)
      else
        ZManaged.fail(None)
  }

  final class JarClassPathEntry(protected val jarReader: JarFileReader[Any, ErrorList]) extends ClassPath with JarLibrary


}
