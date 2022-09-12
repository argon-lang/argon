package dev.argon.plugins.tube.reader

import dev.argon.compiler.*
import dev.argon.io.*
import dev.argon.plugins.tube.{Paths, TubeError}
import dev.argon.util.{*, given}
import scalapb.{GeneratedMessage, GeneratedMessageCompanion}
import zio.stream.ZStream
import zio.*

abstract class TubeZipReaderUtil extends UsingContext {
  val context: Context { type Error >: TubeError }
  protected val zip: ZipFileResource.Zip[context.Env, context.Error]

  protected def getEntryOrError(name: String): ZIO[context.Env & Scope, context.Error, ZipFileResource.Entry[context.Env, context.Error]] =
    zip.getEntry(name).flatMap {
      case Some(entry) => ZIO.succeed(entry)
      case None => ZIO.fail(InvalidTube(s"Missing zip entry: $name"))
    }

  protected def readEntry[A](name: String)(f: BinaryResource[context.Env, context.Error] => Comp[A]): Comp[A] =
    ZIO.scoped(
      getEntryOrError(name)
        .flatMap { entry => f(entry.value) }
    )

  protected def readEntryMessage[A <: GeneratedMessage : GeneratedMessageCompanion](args: Any*): Comp[A] =
    readEntry(Paths.get[A](args *)) { resource =>
      summon[BinaryResourceDecoder[[R, E] =>> ProtobufResource[R, E, A], context.Env, context.Error]]
        .decode(resource)
        .asMessage
    }
}
