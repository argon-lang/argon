package dev.argon.plugins.tube

import dev.argon.compiler.module.ModulePath
import dev.argon.io.*
import dev.argon.plugin.tube.{InvalidTube, SerializedTube}
import dev.argon.tube as t
import dev.argon.util.{*, given}
import scalapb.{GeneratedMessage, GeneratedMessageCompanion}
import zio.stm.{TSet, ZSTM}
import zio.stream.ZStream
import zio.{Scope, ZIO}

class ZipFileTube[R, E >: TubeError](zip: ZipFileResource.Zip[R, E]) extends SerializedTube[R, E] {

  protected def getEntryOrError(name: String): ZIO[R & Scope, E, ZipStreamResource.Entry[R, E]] =
    zip.getEntry(name).flatMap {
      case Some(entry) => ZIO.succeed(entry)
      case None => ZIO.fail(InvalidTube(s"Missing zip entry: $name"))
    }

  protected def readEntry[A](name: String)(f: BinaryResource[R, E] => ZIO[R, E, A]): ZIO[R, E, A] =
    ZIO.scoped(
      getEntryOrError(name)
        .flatMap { entry => f(entry.value) }
    )

  protected def readEntryMessage[A <: GeneratedMessage : GeneratedMessageCompanion](args: Any*): ZIO[R, E, A] =
    readEntry(Paths.get[A](args *)) { resource =>
      summon[BinaryResourceDecoder[[R1, E1] =>> ProtobufResource[R1, E1, A], R, E]]
        .decode(resource)
        .asMessage
    }

  override def version: ZIO[R, E, t.TubeFormatVersion] =
    readEntryMessage()

  override def metadata: ZIO[R, E, t.Metadata] =
    readEntryMessage()

  override def getResource(id: String): ZIO[R, E, FileSystemResource[R, E]] =
    val path = Paths.get[t.Resource](id)
    ZIO.scoped(
      zip.getEntry(path).map(_.isDefined)
    ).flatMap {
      case true =>
        ZIO.succeed(ZipEntryBinaryResource(path))

      case false =>
        val prefix = path + "/"
        zip.entries.filter(_.path.startsWith(prefix))
          .take(1)
          .runCount
          .flatMap {
            case n if n > 0 => ZIO.succeed(ZipDirResource(prefix))
            case _ => ZIO.fail(InvalidTube("Could not find resource: " + id))
          }
    }
  end getResource

  private class ZipEntryBinaryResource(path: String) extends BinaryResource[R, E] with Resource.WithoutFileName {
    override def asBytes: ZStream[R, E, Byte] =
      ZStream.unwrapScoped(
        zip.getEntry(path)
          .flatMap {
            case Some(entry) => ZIO.succeed(entry.value.asBytes)
            case None => ZIO.fail(InvalidTube("Resource went missing: " + path))
          }
      )
  }

  private class ZipDirResource(prefix: String) extends DirectoryResource[R, E, BinaryResource] with Resource.WithoutFileName {
    override def contents: ZStream[R, E, DirectoryEntry[R, E, BinaryResource]] =
      ZStream.unwrap(
        for
          seenDirs <- TSet.empty[String].commit
        yield zip.entries
          .filter(_.path.startsWith(prefix))
          .flatMap { entry =>
            val nextSlash = entry.path.indexOf("/", prefix.length + 1)

            if nextSlash >= 0 then
              ZStream.unwrap(
                seenDirs.contains(entry.path)
                  .tap {
                    case true => ZSTM.unit
                    case false => seenDirs.put(entry.path)
                  }
                  .commit
                  .map {
                    case true => ZStream.empty
                    case false =>
                      val name = entry.path.substring(prefix.length, nextSlash).nn
                      ZStream(DirectoryEntry.Subdirectory(name, ZipDirResource(entry.path + "/")))
                  }
              )
            else
              val name = entry.path.substring(prefix.length).nn
              ZStream(DirectoryEntry.File(name, ZipEntryBinaryResource(entry.path)))
            end if
          }
      )


  }

  override def getModule(modulePath: t.ModulePath): ZIO[R, E, t.ModuleDefinition] =
    val arModulePath = ModulePath(modulePath.name)
    readEntryMessage(arModulePath)
  end getModule

  override def getClass(id: BigInt): ZIO[R, E, t.ClassDefinition] =
    readEntryMessage(id)

  override def getTrait(id: BigInt): ZIO[R, E, t.TraitDefinition] =
    readEntryMessage(id)

  override def getFunction(id: BigInt): ZIO[R, E, t.FunctionDefinition] =
    readEntryMessage(id)

  override def getMethod(id: BigInt): ZIO[R, E, t.MethodDefinition] =
    readEntryMessage(id)

  override def getClassConstructor(id: BigInt): ZIO[R, E, t.ClassConstructorDefinition] =
    readEntryMessage(id)
}
