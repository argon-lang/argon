package dev.argon.plugins.tube.reader

import dev.argon.compiler.*
import dev.argon.compiler.definitions.*
import dev.argon.compiler.tube.TubeImporter
import dev.argon.compiler.vtable.VTableBuilder
import dev.argon.io.ZipFileResource
import dev.argon.plugins.tube.Paths
import dev.argon.tube as t
import dev.argon.tube.given
import dev.argon.util.{*, given}
import zio.*

abstract class TubeReaderFactory extends TubeZipReaderUtil {

  protected val tubeImporter: TubeImporter & HasContext[context.type]

  def create: Comp[TubeReaderBase & HasContext[context.type]] =
    for
      _ <- checkVersion
      metadata <- readEntryMessage[t.Metadata]()
      vtBuilder <- VTableBuilder(TubeReaderFactory.this.context, tubeImporter)
      reader <- metadata.`type` match {
        case t.TubeType.Interface =>
          ZIO.succeed(new TubeReaderCommon(vtBuilder) {
            override type IsImplementation = true

            override protected def asDeclarationImpl[TTube <: ArTube](tube: TTube & HasImplementation[IsImplementation]): Option[TTube & HasImplementation[true]] =
              Some(tube)

            override protected def ensureOptions(opt: Option[context.Options]): Comp[context.Options] =
              ZIO.fromEither(opt.toRight { InvalidTube("Missing options for implementation tube") })


            override protected def createImplementation[TImpl](currentTube: ArTube & HasImplementation[IsImplementation])(f: Option[context.Options] => Comp[Option[TImpl]]): Comp[TImpl] =
              f(Some(currentTube.options)).flatMap {
                case Some(impl) => ZIO.succeed(impl)
                case None => ZIO.fail(InvalidTube("Missing implementation in implementation tube"))
              }

            override protected def getMaybeImplementation[TImpl]: Comp[TImpl] => Comp[Option[TImpl]] =
              _.asSome
          })

        case t.TubeType.Implementation =>
          ZIO.succeed(new TubeReaderCommon(vtBuilder) {
            override type IsImplementation = false

            override protected def asDeclarationImpl[TTube <: ArTube](tube: TTube & HasImplementation[IsImplementation]): Option[TTube & HasImplementation[true]] =
              None

            override protected def ensureOptions(opt: Option[context.Options]): Comp[Unit] =
              ZIO.unit

            override protected def createImplementation[TImpl](currentTube: ArTube & HasImplementation[IsImplementation])(f: Option[context.Options] => Comp[Option[TImpl]]): Comp[Option[TImpl]] =
              f(None)

            override protected def getMaybeImplementation[TImpl]: Comp[Option[TImpl]] => Comp[Option[TImpl]] =
              identity

          })

        case t.TubeType.Unrecognized(_) =>
          ZIO.fail(InvalidTube("Unknown tube type"))
      }
    yield reader


  private def checkVersion: Comp[Unit] =
    for
      version <- readEntryMessage[t.TubeFormatVersion]()
      defaultVersion = t.TubeProto.defaultTubeFormatVersion.get(t.TubeFormatVersion.scalaDescriptor.getOptions).get
      _ <- ZIO.fail(InvalidTube(s"Tube format ${version.major}.${version.minor} is newer than maximum supported version ${defaultVersion.major}.${defaultVersion.minor}"))
        .when(version.major > defaultVersion.major || (version.major == defaultVersion.major && version.minor > defaultVersion.minor))
    yield ()

  private abstract class TubeReaderCommon(override protected val vtableBuilder: VTableBuilder[context.type]) extends TubeReaderBase {
    override val context: TubeReaderFactory.this.context.type = TubeReaderFactory.this.context
    override protected val zip: ZipFileResource.Zip[context.Env, context.Error] = TubeReaderFactory.this.zip
    override protected val tubeImporter: TubeImporter & HasContext[context.type] = TubeReaderFactory.this.tubeImporter
  }

}
